{-# LANGUAGE OverloadedStrings #-}
module Util where

import           Data.Aeson                 (ToJSON)
import qualified Data.Aeson                 as Aeson (encode)
import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as BS (map)
import qualified Data.ByteString.Base64.URL as URL
import qualified Data.ByteString.Lazy       as LBS
import           Data.Char                  (isSpace, ord)
import           Data.List.Split
import           Data.Monoid
import           Data.String.Conv           (toS)
import           Network.HTTP.Types.URI     (urlDecode)

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

renderJsVars :: [(LBS.ByteString, LBS.ByteString)] -> LBS.ByteString
renderJsVars vars =
    "<script>" <> LBS.concat (map (\(var, val) -> var <> " = " <> val <> ";\n") vars) <> "</script>"
