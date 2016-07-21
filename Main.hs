{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

import Control.Exception (try, SomeException)
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

staticRoot :: IsString a => a
staticRoot = "cgc/"

main = do
    let port = 8000
    putStrLn $ "Listening on port " ++ show port
    db <- openDB "database.acid"
    key <- getRandomBytes 32
    run port (app key db)

badRequest = responseLBS status400 [(hContentType, "text/plain")] "what nonsense is this ? baad request daag"
loginFailed = responseLBS status400 [(hContentType, "text/plain")] "Laaagin failed, daaaaag"
cookieResponse cookie = responseLBS status302 
                        [ (hLocation, "/index.html")
                        , ("Set-Cookie", cookieName <> "=" <> cookie <> "; Path=/; HttpOnly;")] -- TODO add secure flag once https
                        ""
cookieName = "cgc_sid"

app :: Key -> DBContext -> Application
app key db req f
    | pathInfo req == [] =
        f $ responseLBS status301 [(hLocation, "/index.html")] ""
    | pathInfo req == ["registerFestival"] = do
        print =<< requestBody req
        f $ responseLBS status200 [(hContentType, "text/plain")] "shit happening yo"
    | pathInfo req == ["register"] = do
        registerReq <- parseRequestBody <$> requestBody req
        mUser <- createUser registerReq
        maybe (putStrLn ("register failed: " ++ (show registerReq)) >> f badRequest) 
            (\user -> do 
                putStrLn $ "registered a user: " ++ (show user)
                runDB db (addUser user)
                cookie <- generateCookie key user
                f $ cookieResponse cookie)
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
        let notFound = f $ responseLBS status404 [] "404 Not Found"
        let path = staticRoot <> intercalate "/" (pathInfo req)
        let filename = last $ pathInfo req
        if "../" `isInfixOf` path then -- no path traversal for you
            notFound
        else do
            result :: Either SomeException LBS.ByteString <- try $ LBS.readFile $ toS path
            case result of
                Left _ ->
                    notFound
                Right contents -> do
                    let mimeType = defaultMimeLookup filename
                    if mimeType == "text/html" then do
                        header <- LBS.readFile (staticRoot <> "header.html")
                        footer <- LBS.readFile (staticRoot <> "footer.html")
                        f $ responseLBS status200 [(hContentType, mimeType)] (header <> contents <> footer)
                    else
                        f $ responseLBS status200 [(hContentType, mimeType)] contents
