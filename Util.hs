{-# LANGUAGE OverloadedStrings #-}
module Util where

import Data.ByteString (ByteString)
import Data.String.Conv (toS)
import Data.List.Split
import Network.HTTP.Types.URI (urlDecode)
import qualified Data.ByteString            as BS (map)
import qualified Data.ByteString.Base64.URL as URL
import Data.Char (ord, isSpace)

parseRequestBody :: ByteString -> [(String, String)]
parseRequestBody b = map f $ map (splitOn "=") $ splitOn "&" $ toS b
    where
        f :: [String] -> (String, String)
        f (a:[]) = (a, "")
        f (a:b:_) = (a, toS $ urlDecode True $ toS b)
        f _ = ("", "")

equalsToTilde, tildeToEquals :: ByteString -> ByteString
equalsToTilde = BS.map (\c -> if c == fromIntegral (ord '=') then fromIntegral $ ord '~' else c)
tildeToEquals = BS.map (\c -> if c == fromIntegral (ord '~') then fromIntegral $ ord '=' else c)

cookieEncode, cookieDecode :: ByteString -> ByteString
cookieEncode = equalsToTilde . URL.encode
cookieDecode = either (const "") id . URL.decode . tildeToEquals

trim :: String -> String
trim = takeWhile (not . isSpace) . dropWhile isSpace

parseCookies :: ByteString -> [(String, String)]
parseCookies = map (\(a:b:_) -> (a,b)) . filter ((>= 2) . length) . map (splitOn "=" . trim) . splitOn ";" . toS
