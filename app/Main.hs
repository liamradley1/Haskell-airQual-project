{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import HTTP --Importing the HTTP library
import Parse -- Importing the Parse library
import Database (initialiseDB , saveRecords , saveParameters , saveMeasurements) -- Importing the Database library
import GHC.Records (getField)
main :: IO ()
main = do
    let urlForPara = "https://raw.githubusercontent.com/liamradley1/functional-assignment/main/parameters.json" -- The url of the localised source from where we are requesting the data from
    jsonPara <- download urlForPara -- Downloads the data
    print jsonPara
    case parseParameters jsonPara of -- Parsing the json file into a list of parameter
      Left errors -> print errors -- If the parsing fails, print the error
      Right para -> do
        -- print . take 3 $ para -- If parsing is successful, print the response
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