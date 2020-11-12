module Main where

import HTTP
import Parse
import Database


-- import qualified  Data.ByteString.Lazy.Char8 as L8
-- import Database.HDBC.PostgreSQL (Connection)
-- saveJSONS :: [L8.ByteString] -> Connection -> IO ()
-- saveJSONS [] _ = do
--     print "Done!"
-- saveJSONS (x:xs) conn = do
--     case parse x of
--         Left err -> print err
--         Right res -> do
--             print "Saving page to DB..." 
--             saveReadings (results res) conn
--     saveJSONS xs conn
--     print "Onto the next page!"

    -- constructJSONS :: [Char] -> [Integer] -> [ByteString] 
    -- constructJSONS urlBase [] = []
    -- constructJSONS urlBase (x:xs) = download (urlBase ++ x) : constructJSONS urlBase xs



main :: IO ()
main = do
    -- let pageNos = [1, 2]
    -- let urlBase = "https://api.openaq.org/v1/measurements?limit=100&page="
    let url1 = "https://api.openaq.org/v1/measurements?limit=100&page=1"
    -- let urls = [urlBase ++ show page  | page <- [1,2]]
    print "Downloading..."
    json1 <- download url1
    print "Parsing ..."
    conn <- initialiseDB
    case parse json1 of
        Left err -> print err
        Right res -> do
            print "Saving to DB..."
            saveReadings (results res) conn
    -- saveJSONS jsons conn
    print "Done!"