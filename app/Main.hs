{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import HTTP --Importing the HTTP library
import Parse -- Importing the Parse library
import Database(initialiseDB,parametersToSqlValue ,recordToSqlValues) -- Importing the Database library
--import Automate(addListToDB, handleInput)
import GHC.Records (getField)

main :: IO ()
main = do
    let url = "https://api.openaq.org/v1/latest?has_geo=true&order_by=city&limit=100" -- ##Change to 10,000 for final
    json <- download url -- Downloads the data
    case (parse json) of -- Parsing the json file into a list of record
      Left errors -> print errors -- If the parsing fails, print the error
      Right record -> do
        print . (take 3) $ (getField @"results" record)-- If parsing is sucessfull, we use getField from GHC.Records to disambiguate which 'results' & print the first 3 records
        print "Initializing Database ..."
        conn <- initialiseDB
        print "Database initialized..."
        print "Records of Record : "
        --recordToSqlValues $ record      --calling this method from Database.hs

    let urlForPara = "https://api.openaq.org/v1/parameters" -- The url of the API from where we are requesting the data from
    jsonPara <- download urlForPara -- Downloads the data
    case (parseParameters jsonPara) of -- Parsing the json file into a list of parameter
      Left errors -> print errors -- If the parsing fails, print the error
      Right para -> do
        print para -- If parsing is successful, print the response
        print "Initializing Database ..."
        conn <- initialiseDB
        print "Database initialized..."
        print "Records of parameter : "
        --parametersToSqlValue $ para --calling this method from Database.hs


    --print json

    -- maxNo <- handleInput
    -- let urlBase = "https://api.openaq.org/v1/measurements?"
    -- if maxNo <= 10000 then do
    --     let urlWithLim = urlBase ++ "limit=" ++ show maxNo
    --     let urls = [urlWithLim]
    --     print "Downloading..."
    --     let jsons = downloadList urls
    --     print "Parsing ..."
    --     conn <- initialiseDB
    --     addListToDB jsons conn
    -- else do -- Need to actually get it to select the right number of entries.
    --     let quot = div maxNo 10000 + 1
    --     let pagenos = [1..quot]
    --     let urls = [urlBase ++ "limit=10000&page=" ++ show nums | nums <- pagenos]
    --     let rem = mod maxNo 10000
    --     print rem
    --     print urls
    --     let jsons = downloadList urls
    --     print "Parsing..."
    --     conn <- initialiseDB
    --     addListToDB jsons conn
