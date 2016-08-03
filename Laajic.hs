{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric      #-}
module Laajic where

import DB
import Data.ByteString (ByteString)
import Data.String.Conv (toS)
import Crypto.Random (getRandomBytes)
import           Crypto.Cipher.AES.Util     (cbcDecrypt', cbcEncrypt')
import qualified Crypto.KDF.PBKDF2 as PBKDF2 (generate)
import Crypto.Hash (SHA256(..))
import Crypto.KDF.PBKDF2 (PRF, prfHMAC, Parameters(..))
import           Data.Time.Format           (defaultTimeLocale, formatTime,
                                             parseTimeM)
import           Data.Time.Clock            (diffUTCTime, getCurrentTime)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson (decode, encode)
import Data.Aeson.Lens
import Control.Lens
import Control.Monad
import Data.Monoid
import Network.Wreq
import Util
import Common

type Salt = ByteString

hashPassword :: Salt -> String -> ByteString
hashPassword salt password =
    PBKDF2.generate (prfHMAC SHA256 :: PRF ByteString) 
        (Parameters { iterCounts = 4000, outputLength = 32 }) (toS password) salt

hashNewPassword :: String -> IO PasswordHash
hashNewPassword password = do
    salt :: ByteString <- getRandomBytes 16
    return $ PasswordHash salt (hashPassword salt password)

validatePasswordHash :: PasswordHash -> String -> Bool
validatePasswordHash (PasswordHash salt hash) password =
    hashPassword salt password == hash -- OOOOOO TIMING ATTACKKK!!!

createUser :: [(String, String)] -> IO (Maybe User)
createUser req = do
    maybe (return Nothing) (\password -> do
        passwordHash <- hashNewPassword password
        return (User <$> lookup "userFirstName" req
                     <*> lookup "userLastName" req
                     <*> lookup "userEmail" req
                     <*> return passwordHash
                     <*> return False)) (lookup "userPassword" req)

-- TODO check for failure and return Bool
sendVerificationEmail :: ByteString -> Key -> User -> IO ()
sendVerificationEmail mailgunKey key user = do
    activationLink_ <- activationLink key user
    let authVal = basicAuth "api" mailgunKey
    let opts = defaults & auth ?~ authVal
    void $ postWith opts "https://api.mailgun.net/v3/classicalguitarcalendar.com/messages"
        [ ("from" :: ByteString, "Classical Guitar Calendar <noreply@classicalguitarcalendar.com>" :: ByteString)
        , ("to", toS $ _email user)
        , ("subject", "Activate your account")
        , ("text", "Hey " <> toS (_firstName user) <> " " <> toS (_lastName user) <>
            ",\nActivate your account by visiting this link: " <> activationLink_)
        , ("html", "<p>Hey " <> toS (_firstName user) <> " " <> toS (_lastName user) <>
            ",\nActivate your account by visiting this <a href=\"" <> activationLink_ <> "\">link</a></p>")
        ]

createFestival :: String -> CookieJSON -> Maybe Festival
createFestival rawJSON cookieJSON = do
    let mEventName = (^? _String) =<< (rawJSON ^? key "eventName")
    maybe Nothing
        (\eventName -> Just $ Festival {
            _ownerEmail = Laajic.email cookieJSON
          , _festivalName = toS eventName
          , _rawJSON = rawJSON
        })
        mEventName

type Key = ByteString

data AccountType =
    Native Bool -- isActivated?
  | Google
  | Facebook
    deriving (Generic, ToJSON, FromJSON)

data CookieJSON = CookieJSON {
    firstName :: String
  , lastName :: String
  , email :: String
  , accountType :: AccountType
  , creationTime :: String
} deriving (Generic, ToJSON, FromJSON)

deriving instance Generic Festival
deriving instance ToJSON Festival
deriving instance FromJSON Festival

isActivated :: CookieJSON -> Bool
isActivated cookieJSON = f (accountType cookieJSON)
    where
        f (Native act) = act
        f Google = True
        f Facebook = True

data ActivationLinkJSON = ActivationLinkJSON {
    actEmail :: String
  , actCreationTime :: String
} deriving (Generic, ToJSON, FromJSON)

activationLink :: Key -> User -> IO ByteString
activationLink key user = do
    currentTime :: ByteString <- toS . formatTime defaultTimeLocale "%s" <$> getCurrentTime
    mToken <- cbcEncrypt' key $ toS $ Aeson.encode $ ActivationLinkJSON (_email user) (toS currentTime)
    either (\s -> error $ "fatal: cbcEncrypt' failed with: " <> s)
        (\token -> return (serverBaseUrl <> "activate?token=" <> (cookieEncode token)))
        mToken

validateActivationToken :: Key -> ByteString -> IO (Maybe String)
validateActivationToken key token = do
    let eightHours = 8 * 60 * 60
    currentTime <- getCurrentTime
    let mToken = either (const Nothing) Just $ cbcDecrypt' key $ cookieDecode token
    maybe (return Nothing) 
        (\activationLinkJSON -> do
            let mCreationTime = parseTimeM True defaultTimeLocale "%s" $ toS $ actCreationTime activationLinkJSON
            maybe (return Nothing)
                (\creationTime_ -> 
                    if currentTime `diffUTCTime` creationTime_ < eightHours then
                        return (Just $ actEmail activationLinkJSON)
                    else
                        return Nothing) 
                mCreationTime)
        (Aeson.decode . toS =<< mToken)

generateCookie :: Key -> AccountType -> (String, String, String) -> IO ByteString
generateCookie key acType (fn, ln, em) = do
    currentTime :: ByteString <- toS . formatTime defaultTimeLocale "%s" <$> getCurrentTime
    mCookie <- cbcEncrypt' key $ toS $ Aeson.encode $ CookieJSON fn ln em acType (toS currentTime)
    either (\s -> error $ "fatal: cbcEncrypt' failed with: " <> s) (return . cookieEncode) mCookie -- TODO make this non-fatal (return Either)

generateNativeCookie :: Key -> User -> IO ByteString
generateNativeCookie key (User fn ln em _ act) =
    generateCookie key (Native act) (fn, ln, em)

validateCookie :: Key -> ByteString -> IO (Maybe CookieJSON)
validateCookie key c = do
    let oneWeek = 7 * 24 * 60 * 60
    currentTime <- getCurrentTime
    let mCookie = either (const Nothing) Just $ cbcDecrypt' key $ cookieDecode c
    maybe (return Nothing) 
        (\cookieJSON -> do
            let mCreationTime = parseTimeM True defaultTimeLocale "%s" $ toS $ creationTime cookieJSON
            maybe (return Nothing)
                (\creationTime_ -> 
                    if currentTime `diffUTCTime` creationTime_ < oneWeek then
                        return (Just cookieJSON)
                    else
                        return Nothing) 
                mCreationTime)
        (Aeson.decode . toS =<< mCookie)
