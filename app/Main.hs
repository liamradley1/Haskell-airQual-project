{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import HTTP --Importing the HTTP library
import Parse -- Importing the Parse library
import Database (initialiseDB , saveRecords , saveParameters , saveMeasurements) -- Importing the Database library
import GHC.Records (getField)
import DataTypes

main :: IO ()
main = do
    let urlForPara = "https://raw.githubusercontent.com/liamradley1/functional-assignment/main/parameters.json" -- The url of the localised source from where we are requesting the data from
    jsonPara <- download urlForPara -- Downloads the data
    print jsonPara
    case parseParameters jsonPara of -- Parsing the json file into a list of parameter
      Left errors -> print errors -- If the parsing fails, print the error
      Right para -> do
        print para -- If parsing is successful, print the response
        print "Initializing Database ..."
        conn <- initialiseDB
        print "Database initialized..."
        saveParameters (getField @"results" para) conn
        print "Parameters saved into db"
        print "Done!"

    let url = "https://raw.githubusercontent.com/liamradley1/functional-assignment/main/measurements.json"
    json <- download url -- Downloads the data
    case parse json of -- Parsing the json file into a list of record
      Left errors -> print errors -- If the parsing fails, print the error
      Right record -> do
        let results = getField @"results" record
        -- print . take 3 $ results-- If parsing is successful, we use getField from GHC.Records to disambiguate which 'results' & print the first 3 records
        print "Connecting to database ..."
        conn <- initialiseDB
        print "Database initialized..."
        saveRecords results conn
        saveMeasurements results conn
        print "Records and Measurements saved into db" 
   ------------------------------------------previous codes-----------------------------------------------------
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
