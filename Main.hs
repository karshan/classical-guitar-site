{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Common
import           Control.Exception         (SomeException, try)
import           Control.Lens
import           Control.Monad
import           Crypto.Random             (getRandomBytes)
import qualified Data.Aeson                as Aeson (encode)
import           Data.Aeson.Lens
import           Data.ByteString           (ByteString)
import qualified Data.ByteString.Lazy      as LBS
import           Data.List                 (find)
import           Data.Monoid
import           Data.String
import           Data.String.Conv          (toS)
import           Data.Text                 (intercalate, isInfixOf)
import           DB
import           Laajic
import           Network.HTTP.Types
import           Network.HTTP.Types.Header
import           Network.Mime
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wreq              hiding (cookieName)
import           Util

staticRoot :: IsString a => a
staticRoot = "cgc/"

main = do
    let port = 8000
    putStrLn $ "Listening on port " ++ show port
    db <- openDB "database.acid"
    key <- getRandomBytes 32
    googCreds <- (\[a,b] -> (a,b)) . map toS . lines <$> readFile "googleoauthcreds"
    facebookCreds <- (\[a,b] -> (a,b)) . map toS . lines <$> readFile "facebookoauthcreds"
    mailgunKey <- toS . head . lines <$> readFile "mailgunkey"
    runSettings (setPort port $ setHost "127.0.0.1" defaultSettings) (app mailgunKey googCreds facebookCreds key db)

badRequest = responseLBS status400 [(hContentType, "text/plain")] "what nonsense is this ? baad request daag"
userExists = responseLBS status400 [(hContentType, "text/plain")] "user with this email already exists"
userDoesNotExist email = responseLBS status400 [(hContentType, "text/plain")] ("user " <> email <> " doesn't exist")
verifyEmail = responseLBS status200 [(hContentType, "text/plain")] "Please verify your email by clicking the link in the email we sent you"
accountActivated email = responseLBS status200 [(hContentType, "text/plain")] (email <> ", you're account has been activated")
notFound = responseLBS status404 [] "404 Not Found"
festivalExists = responseLBS status400 [(hContentType, "text/plain")] "festival with this name already exists"
loginFailed = responseLBS status403 [(hContentType, "text/plain")] "Laaagin failed, daaaaag"
notAuthorized = responseLBS status403 [(hContentType, "text/plain")] "Gatta laagin first daaag"
cookieResponse cookie =
    responseLBS status302
        [ (hLocation, "/index.html")
        , ("Set-Cookie", cookieName <> "=" <> cookie <> "; Path=/; Secure; HttpOnly;")]
        ""

cookieName :: (IsString a) => a
cookieName = "cgc_sid"

googleOauthRedirectUri :: (Monoid a, IsString a) => a
googleOauthRedirectUri = serverBaseUrl <> "googleoauth"

facebookOauthRedirectUri :: (Monoid a, IsString a) => a
facebookOauthRedirectUri = serverBaseUrl <> "facebookoauth"

getCookieJSON :: Key -> Request -> IO (Maybe CookieJSON)
getCookieJSON key req = do
    let mCookie = (do
            cookieHeader <- lookup hCookie (requestHeaders req)
            lookup cookieName $ parseCookies cookieHeader)
    maybe (return Nothing)
        (\cookie -> validateCookie key $ toS cookie)
        mCookie

type GoogOauthCreds = (ByteString, ByteString)
type FacebookOauthCreds = (ByteString, ByteString)

app :: ByteString -> GoogOauthCreds -> FacebookOauthCreds -> Key -> DBContext -> Application
app mailgunKey (googClientId, googClientSecret) (facebookClientId, facebookClientSecret) encryptionKey db req f
    | pathInfo req == [] =
        f $ responseLBS status301 [(hLocation, "/index.html")] ""
    | head (pathInfo req) == "festivals" && length (pathInfo req) == 2 = do
        let reqFestivalName = (pathInfo req) !! 1
        festivals <- runDB db getFestivals
        let mFestival = find ((toS reqFestivalName ==) . _URISafeName) festivals
        maybe (f notFound)
            (\festival -> do
                header <- LBS.readFile (staticRoot <> "header.html")
                footer <- LBS.readFile (staticRoot <> "footer.html")
                contents <- LBS.readFile (staticRoot <> "descriptionPrototype.html")
                mCookieJSON <- getCookieJSON encryptionKey req
                let jsVars =
                        [("festival", Aeson.encode festival)] <>
                            maybe []
                                (\cookieJSON -> [("user", Aeson.encode cookieJSON)])
                                mCookieJSON
                f $ responseLBS status200 [(hContentType, "text/html")] (header <> renderJsVars jsVars <> contents <> footer))
            mFestival
    | head (pathInfo req) == "edit" && length (pathInfo req) == 2 = do
        mCookieJSON <- getCookieJSON encryptionKey req
        let reqFestivalName = (pathInfo req) !! 1
        --TODO
        f badRequest
    | pathInfo req == ["registerFestival"] = do
        mCookieJSON <- getCookieJSON encryptionKey req
        maybe (f notAuthorized)
            (\cookieJSON ->
                if isActivated cookieJSON == False then
                    f notAuthorized
                else do
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
                mExistingUser <- runDB db (addUser user)
                maybe
                    (do
                        sendVerificationEmail mailgunKey encryptionKey user
                        cookie <- generateNativeCookie encryptionKey user
                        f $ cookieResponse cookie)
                    (const $ f userExists)
                    mExistingUser)
            mUser
    | pathInfo req == ["activate"] = do
        maybe (f badRequest)
            (\token -> do
                mEmail <- validateActivationToken encryptionKey token
                maybe (f badRequest)
                    (\email -> do
                        runDB db (activateUser email)
                        f $ accountActivated $ toS email) -- TODO account activated page and set cookies here
                    mEmail)
            (join (lookup "token" (queryString req)))
    | pathInfo req == ["contact"] = do
        contactReq <- parseRequestBody <$> requestBody req
        let mContactForm = createContactForm contactReq
        maybe (f badRequest)
            (\contactForm -> do
                sendContactForm mailgunKey contactForm
                f $ responseLBS status302 [(hLocation, "/contact_form_sent.html")] "")
            mContactForm
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
                            cookie <- generateNativeCookie encryptionKey user
                            f $ cookieResponse cookie
                        else
                            f loginFailed)
                    mUser)
            mEmailPass
    | pathInfo req == ["forgotpassword"] = do
        fpReq <- parseRequestBody <$> requestBody req
        let mEmail = lookup "forgot-password-email" fpReq
        maybe (f badRequest)
            (\email -> do
                users <- runDB db getUsers
                let mUser = find ((== email) . _email) users
                maybe (f $ userDoesNotExist $ toS email)
                    (\user -> do
                        sendResetPasswordEmail mailgunKey encryptionKey user
                        f $ responseLBS status302 [(hLocation, "/reset_password_link_sent.html")] "")
                    mUser)
            mEmail
    | pathInfo req == ["googleoauth"] = do
        -- TODO error logging
        -- TODO disable wreq exceptions (but log them ?)
        -- TODO use display name instead of firstname last name ??
        maybe (f badRequest)
            (\code -> do
                codeResp <- post "https://www.googleapis.com/oauth2/v4/token"
                    [ ("code" :: ByteString, code)
                    , ("client_id", googClientId)
                    , ("client_secret", googClientSecret)
                    , ("redirect_uri", googleOauthRedirectUri)
                    , ("grant_type", "authorization_code")
                    ]
                let mToken = (\(json :: ByteString) -> (json ^? key "access_token") >>= (^? _String)) $ toS $ codeResp ^. responseBody
                maybe (f badRequest)
                    (\accessToken -> do
                        emailResp <- (toS . (^. responseBody)) <$> get
                            ("https://www.googleapis.com/plus/v1/people/me?access_token=" <> toS accessToken)
                        let mEmail = toS <$> ((\(json :: ByteString) -> (json ^? key "emails") >>=
                                (^? _Array) >>=
                                (^? ix 0) >>=
                                (^? key "value") >>=
                                (^? _String)) emailResp)
                        let mFirstName = toS <$> ((\(json :: ByteString) -> (json ^? key "name") >>=
                                (^? key "givenName") >>=
                                (^? _String)) emailResp)
                        let mLastName = toS <$> ((\(json :: ByteString) -> (json ^? key "name") >>=
                                (^? key "familyName") >>=
                                (^? _String)) emailResp)
                        maybe (f badRequest)
                            (\userInfo -> do
                                cookie <- generateCookie encryptionKey Google userInfo
                                f $ cookieResponse cookie)
                            ((,,) <$> mFirstName <*> mLastName <*> mEmail))
                    mToken)
            (join (lookup "code" (queryString req)))
    | pathInfo req == ["facebookoauth"] = do
        -- TODO error logging
        -- TODO disable wreq exceptions (but log them ?)
        -- TODO use name instead of first_name+last_name ?
        maybe (f badRequest)
            (\code -> do
                codeResp <- get ("https://graph.facebook.com/v2.3/oauth/access_token?client_id=" <>
                    toS facebookClientId <>
                    "&client_secret=" <> toS facebookClientSecret <>
                    "&redirect_uri=" <> facebookOauthRedirectUri <>
                    "&code=" <> toS code)
                let mToken = (\(json :: ByteString) -> (json ^? key "access_token") >>= (^? _String)) $ toS $ codeResp ^. responseBody
                maybe (f badRequest)
                    (\accessToken -> do
                        emailResp <- (toS . (^.responseBody)) <$> get
                            ("https://graph.facebook.com/v2.3/me?fields=email,name,first_name,last_name&access_token=" <>
                                toS accessToken)
                        let mEmail = toS <$> ((\(json :: ByteString) -> (json ^? key "email") >>=
                                (^? _String)) emailResp)
                        let mFirstName = toS <$> ((\(json :: ByteString) -> (json ^? key "first_name") >>=
                                (^? _String)) emailResp)
                        let mLastName = toS <$> ((\(json :: ByteString) -> (json ^? key "last_name") >>=
                                (^? _String)) emailResp)
                        maybe (f badRequest)
                            (\userInfo -> do
                                cookie <- generateCookie encryptionKey Facebook userInfo
                                f $ cookieResponse cookie)
                            ((,,) <$> mFirstName <*> mLastName <*> mEmail))
                    mToken)
            (join (lookup "code" (queryString req)))
    | pathInfo req == ["logout"] = do
        f $ responseLBS status302
                [ (hLocation, "/index.html")
                , ("Set-Cookie", cookieName <> "= deleted; Path=/; Secure; HttpOnly; expires=Thu, 01 Jan 1970 00:00:00 GMT") ]
                ""
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
                        mCookieJSON <- getCookieJSON encryptionKey req
                        festivals <- runDB db getFestivals -- TODO only for index.html ?
                        let jsVars =
                                [("festivals", Aeson.encode festivals)] <>
                                    maybe []
                                        (\cookieJSON -> [("user", Aeson.encode cookieJSON)])
                                        mCookieJSON
                        f $ responseLBS status200 [(hContentType, mimeType)] (header <> renderJsVars jsVars <> contents <> footer)
                    else
                        f $ responseLBS status200 [(hContentType, mimeType)] contents
