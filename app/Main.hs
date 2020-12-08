{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import HTTP --Importing the HTTP library
import Parse -- Importing the Parse library
import Database (initialiseDB, saveRecords, saveParameters, saveMeasurements, getConn, dropDB, joinFromSqlValues,
  selectLocations, deleteLocations, joinParameterAndMeasurement, measurementsFromSqlValues) -- Importing the Database library
import GHC.Records (getField)
import System.Environment

main :: IO ()
main = do
  dropDB
  let urlForPara =  "https://raw.githubusercontent.com/liamradley1/functional-assignment/main/parameters.json" -- The url of the localised source from where we are requesting the data from
  jsonPara <- download urlForPara -- Downloads the data
  case parseParameters jsonPara of -- Parsing the json file into a list of parameter
    Left errors -> print errors -- If the parsing fails, print the error
    Right para -> do
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
      print "Connecting to database ..."
      conn <- initialiseDB
      print "Database initialized..."
      saveRecords results conn
      saveMeasurements results conn
      print "Records and Measurements saved into db" 

  args <- getArgs
  conn <- getConn
  case args of
    ["selectLocations"] -> do
      locations <- selectLocations conn
      measurementsFromSqlValues locations 1
    ["deleteLocations"] -> do
      print "Input the country you want to delete."
      toDelete <- getLine 
      deleteLocations conn toDelete
      print "Deleted."
    ["joinParameterAndMeasurement"] -> do
      res <- joinParameterAndMeasurement conn
      joinFromSqlValues res 1
    _ -> do 
      print "Usage: System command [args]"
      print "selectLocations                                  Selects all locations in ascending order, from locations table"
      print "deleteLocations                                  Deletes countries, based on user input"
      print "joinParameterAndMeasurement                      Joins Parameter and Measurement table to retrieve some data"
   