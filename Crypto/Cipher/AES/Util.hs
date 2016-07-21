-- This module exports functions convenient for encrypting and decrypting strings
-- that don't mind whitespace being tacked onto their bottoms
module Crypto.Cipher.AES.Util
    (
      cbcEncrypt'
    , cbcDecrypt'
    ) where

import           Control.Lens           (over, _Left)
import           Control.Monad          (when)
import           Crypto.Cipher.AES      (AES256)
import           Crypto.Cipher.Types    (KeySizeSpecifier (..), blockSize,
                                         cbcDecrypt, cbcEncrypt, cipherInit,
                                         cipherKeySize, makeIV)
import           Crypto.Error           (CryptoFailable, eitherCryptoError)
import           Crypto.Hash.Algorithms (SHA256)
import           Crypto.Hash.IO         (hashBlockSize, hashDigestSize)
import qualified Crypto.KDF.Scrypt      as Scrypt (Parameters (..), generate)
import           Crypto.MAC.HMAC        (HMAC, hmac, hmacGetDigest)
import           Crypto.Random          (getRandomBytes)
import qualified Data.ByteArray         as BA (unpack)
import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as BS (length, pack, replicate, splitAt)
import           Data.Monoid            ((<>))

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither _ (Just a) = Right a
maybeToEither e Nothing  = Left e

sha256BlockSize :: Int
sha256BlockSize = hashBlockSize (undefined :: SHA256)

aes256BlockSize :: Int
aes256BlockSize = blockSize (undefined :: AES256)

aes256KeySize :: Int
aes256KeySize = f $ cipherKeySize (undefined :: AES256)
    where
        f :: KeySizeSpecifier -> Int
        f (KeySizeRange _ a) = a
        f (KeySizeEnum xs) = maximum xs
        f (KeySizeFixed a) = a

scryptParams :: Int -- ^ Output Size
             -> Scrypt.Parameters
scryptParams = Scrypt.Parameters (2 ^ (14 :: Integer)) 8 1

-- Pads with spaces
aes256pad :: ByteString -> ByteString
aes256pad b = b <> BS.replicate (aes256BlockSize - (BS.length b `mod` aes256BlockSize)) 32

genKeys :: ByteString               -- ^ passphrase
        -> ByteString               -- ^ salt
        -> (ByteString, ByteString) -- ^ (AES256Key, hmacSHA256Key)
genKeys passphrase salt = BS.splitAt aes256KeySize $
                                     Scrypt.generate (scryptParams $ aes256KeySize + sha256BlockSize)
                                                     passphrase
                                                     salt

cbcEncrypt' :: ByteString -> ByteString -> IO (Either String ByteString)
cbcEncrypt' passphrase plainText = do -- IO
    rawIv <- getRandomBytes aes256BlockSize
    return (do -- Either
        let (encKey, hmacKey) = genKeys passphrase rawIv
        cipher <- over _Left show $ eitherCryptoError (cipherInit encKey :: CryptoFailable AES256)
        iv <- maybeToEither "makeIV failed" $ makeIV rawIv
        let ivCipherText = rawIv <> cbcEncrypt cipher iv (aes256pad plainText)
        return $ ivCipherText <> (BS.pack . BA.unpack . hmacGetDigest) (hmac hmacKey ivCipherText :: HMAC SHA256))

cbcDecrypt' :: ByteString -> ByteString -> Either String ByteString
cbcDecrypt' passphrase ivCipherTextMac = do
    let (ivCipherText, mac) = BS.splitAt (BS.length ivCipherTextMac - hashDigestSize (undefined :: SHA256)) ivCipherTextMac
    let (rawIv, cipherText) = BS.splitAt aes256BlockSize ivCipherText
    let (encKey, hmacKey) = genKeys passphrase rawIv
    let computedMac = (BS.pack . BA.unpack . hmacGetDigest) (hmac hmacKey ivCipherText :: HMAC SHA256)
    when (mac /= computedMac) -- OOOO Timing Attack!!
         (Left "HMAC Verification Failed")
    cipher <- over _Left show $ eitherCryptoError (cipherInit encKey :: CryptoFailable AES256)
    iv <- maybeToEither "makeIV failed" $ makeIV rawIv
    return $ cbcDecrypt cipher iv cipherText
