{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

import Control.Exception (try, SomeException)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.List (find)
import Data.Monoid
import Data.String
import Data.String.Conv (toS)
import Data.Text (intercalate, isInfixOf)
import Network.Mime
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types
import Network.HTTP.Types.Header
import Util
import DB
import Laajic
import Crypto.Random (getRandomBytes)
import qualified Data.Aeson as Aeson (encode)

staticRoot :: IsString a => a
staticRoot = "cgc/"

main = do
    let port = 8000
    putStrLn $ "Listening on port " ++ show port
    db <- openDB "database.acid"
    key <- getRandomBytes 32
    run port (app key db)

badRequest = responseLBS status400 [(hContentType, "text/plain")] "what nonsense is this ? baad request daag"
userExists = responseLBS status400 [(hContentType, "text/plain")] "user with this email already exists"
notFound = responseLBS status404 [] "404 Not Found"
festivalExists = responseLBS status400 [(hContentType, "text/plain")] "festival with this name already exists"
loginFailed = responseLBS status403 [(hContentType, "text/plain")] "Laaagin failed, daaaaag"
notAuthorized = responseLBS status403 [(hContentType, "text/plain")] "Gatta laagin first daaag"
cookieResponse cookie = responseLBS status302 
                        [ (hLocation, "/index.html")
                        , ("Set-Cookie", cookieName <> "=" <> cookie <> "; Path=/; HttpOnly;")] -- TODO add secure flag once https
                        ""
cookieName :: (IsString a) => a
cookieName = "cgc_sid"

getCookieJSON :: Key -> Request -> IO (Maybe CookieJSON)
getCookieJSON key req = do
    let mCookie = (do
            cookieHeader <- lookup hCookie (requestHeaders req)
            lookup cookieName $ parseCookies cookieHeader)
    maybe (return Nothing)
        (\cookie -> validateCookie key $ toS cookie)
        mCookie

app :: Key -> DBContext -> Application
app key db req f
    | pathInfo req == [] =
        f $ responseLBS status301 [(hLocation, "/index.html")] ""
    | head (pathInfo req) == "festivals" && length (pathInfo req) == 2 = do
        let reqFestivalName = (pathInfo req) !! 1
        festivals <- runDB db getFestivals
        let mFestival = find ((toS reqFestivalName ==) . _festivalName) festivals
        maybe (f notFound)
            (\festival -> do
                header <- LBS.readFile (staticRoot <> "header.html")
                footer <- LBS.readFile (staticRoot <> "footer.html")
                contents <- LBS.readFile (staticRoot <> "descriptionPrototype.html")
                mCookieJSON <- getCookieJSON key req
                let jsVars =
                        maybe "" 
                            (\cookieJSON -> 
                                "<script>user = " <>
                                    Aeson.encode cookieJSON <> ";\n" <>
                                    "festival = " <> Aeson.encode festival <>
                                    ";</script>")
                            mCookieJSON
                f $ responseLBS status200 [(hContentType, "text/html")] (header <> jsVars <> contents <> footer)

                )
            mFestival
    | pathInfo req == ["registerFestival"] = do
        mCookieJSON <- getCookieJSON key req
        maybe (f notAuthorized)
            (\cookieJSON -> do
                rawReq <- toS <$> requestBody req
                let mFestival = createFestival rawReq cookieJSON
                maybe (f badRequest)
                    (\festival -> do
                        success <- runDB db (addFestival festival)
                        if success then
                            f $ responseLBS status200 [(hContentType, "text/plain")] "Festival Added"
                        else
                            f festivalExists)
                    mFestival)
            mCookieJSON
    | pathInfo req == ["register"] = do
        registerReq <- parseRequestBody <$> requestBody req
        mUser <- createUser registerReq
        maybe (putStrLn ("register failed: " ++ (show registerReq)) >> f badRequest) 
            (\user -> do 
                success <- runDB db (addUser user)
                if success then do
                    cookie <- generateCookie key user
                    f $ cookieResponse cookie
                else
                    f userExists)
            mUser
    | pathInfo req == ["login"] = do
        loginReq <- parseRequestBody <$> requestBody req
        let mEmailPass =
                (,) <$> lookup "userEmail" loginReq
                    <*> lookup "userPassword" loginReq
        maybe (f badRequest)
            (\(email, password) -> do
                users <- runDB db getUsers
                let mUser = find ((== email) . _email) users
                maybe (f loginFailed)
                    (\user ->
                        if validatePasswordHash (_passwordHash user) password then do
                            cookie <- generateCookie key user
                            f $ cookieResponse cookie
                        else
                            f loginFailed)
                    mUser)
            mEmailPass
    | otherwise = do
        let path = staticRoot <> intercalate "/" (pathInfo req)
        let filename = last $ pathInfo req
        if "../" `isInfixOf` path then -- no path traversal for you
            f notFound
        else do
            result :: Either SomeException LBS.ByteString <- try $ LBS.readFile $ toS path
            case result of
                Left _ ->
                    f notFound
                Right contents -> do
                    let mimeType = defaultMimeLookup filename
                    if mimeType == "text/html" then do
                        header <- LBS.readFile (staticRoot <> "header.html")
                        footer <- LBS.readFile (staticRoot <> "footer.html")
                        mCookieJSON <- getCookieJSON key req
                        let jsVars =
                                maybe "" 
                                    (\cookieJSON -> 
                                        "<script>user = " <>
                                            Aeson.encode cookieJSON <>
                                            ";</script>")
                                    mCookieJSON
                        f $ responseLBS status200 [(hContentType, mimeType)] (header <> jsVars <> contents <> footer)
                    else
                        f $ responseLBS status200 [(hContentType, mimeType)] contents
