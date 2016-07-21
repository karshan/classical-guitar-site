{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
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
import Util

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
                     <*> return passwordHash)) (lookup "userPassword" req)

type Key = ByteString

data CookieJSON = CookieJSON {
    firstName :: String
  , lastName :: String
  , email :: String
  , creationTime :: String
} deriving (Generic, ToJSON, FromJSON)

generateCookie :: Key -> User -> IO ByteString
generateCookie key (User fn ln em _) = do
    currentTime :: ByteString <- toS . formatTime defaultTimeLocale "%s" <$> getCurrentTime
    mCookie <- cbcEncrypt' key $ toS $ Aeson.encode $ CookieJSON fn ln em (toS currentTime)
    either (error "fatal: cbcEncrypt' failed with: ") (return . cookieEncode) mCookie

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