{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

import Control.Exception (try, SomeException)
import qualified Data.ByteString.Lazy as LBS
import Data.Monoid
import Data.String
import Data.String.Conv (toS)
import Data.Text (intercalate)
import Network.Mime
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types
import Network.HTTP.Types.Header

staticRoot :: IsString a => a
staticRoot = "cgc/"

main = do
    let port = 8000
    putStrLn $ "Listening on port " ++ show port
    run port app

app :: Application
app req f
    | pathInfo req == [] =
        f $ responseLBS status301 [(hLocation, "/index.html")] ""
    | pathInfo req == ["registerFestival"] = do
        print =<< requestBody req
        f $ responseLBS status200 [(hContentType, "text/plain")] "shit happening yo"
    | otherwise = do
        -- FIXME path traversal
        let path = staticRoot <> intercalate "/" (pathInfo req)
        let filename = last $ pathInfo req
        result :: Either SomeException LBS.ByteString <- try $ LBS.readFile $ toS path
        case result of
            Left _ ->
                f $ responseLBS status404 [] "Naat faaound daagie!"
            Right contents -> do
                let mimeType = defaultMimeLookup filename
                if mimeType == "text/html" then do
                    header <- LBS.readFile (staticRoot <> "header.html")
                    footer <- LBS.readFile (staticRoot <> "footer.html")
                    f $ responseLBS status200 [(hContentType, mimeType)] (header <> contents <> footer)
                else
                    f $ responseLBS status200 [(hContentType, mimeType)] contents
