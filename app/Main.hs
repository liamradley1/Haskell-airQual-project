module Main where

import Automate(addListToDB, handleInput)
import HTTP
import Database

main :: IO ()
main = do
    maxNo <- handleInput
    let urlBase = "https://api.openaq.org/v1/measurements?"
    if maxNo <= 10000 then do
        let urlWithLim = urlBase ++ "limit=" ++ show maxNo
        let urls = [urlWithLim]
        print "Downloading..."
        let jsons = downloadList urls
        print "Parsing ..."
        conn <- initialiseDB
        addListToDB jsons conn
    else do -- Need to actually get it to select the right number of entries.
        let quot = div maxNo 10000 + 1
        let pagenos = [1..quot]
        let urls = [urlBase ++ "limit=10000&page=" ++ show nums | nums <- pagenos]
        let rem = mod maxNo 10000
        print rem
        print urls
        let jsons = downloadList urls
        print "Parsing..."
        conn <- initialiseDB
        addListToDB jsons conn
        
        
