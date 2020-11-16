module HTTP (downloadList) where

import qualified Data.ByteString.Lazy.Char8 as L8
import Network.HTTP.Simple

type URL = String

download:: URL -> IO L8.ByteString
download url = do
    request <- parseRequest url
    response <- httpLBS request
    return $ getResponseBody response

    
downloadList :: [URL] -> [IO L8.ByteString]
downloadList = map download 
    