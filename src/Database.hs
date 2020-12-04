{-# LANGUAGE DeriveGeneric #-} -- Language extention
{-# LANGUAGE DuplicateRecordFields #-} -- Using DuplicateRecordFields allows the records to use duplicate field labels.
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-|
Module      : Database
Description : Provides functionality for initialisation of the database, conversion of data types to SqlValues, and insertion of SqlValues to the database. 

Provides functionality for initialisation of the database, conversion of data types to SqlValues, and insertion of SqlValues to the database.
Ensure that the initialiseDB function is altered to match the PostgreSQL login details for your machine.
-}

    module Database(
        initialiseDB , saveRecords , saveParameters , saveMeasurements
    ) where

import Database.HDBC
import Database.HDBC.PostgreSQL
import DataTypes
import Parse

-- | An IO function that initialises the databases if they do not already exist. Returns an IO Connection
initialiseDB :: IO Connection 
initialiseDB = do
        conn <- connectPostgreSQL "host=localhost dbname = airqual_db user=postgres password =admin"
        run conn "CREATE TABLE IF NOT EXISTS locations (\
            \locationId SERIAL NOT NULL PRIMARY KEY,\
            \location VARCHAR(100) NOT NULL,\
            \city VARCHAR(100) NOT NULL,\
            \country VARCHAR(100) NOT NULL,\
            \latitude NUMERIC(10,6) NOT NULL,\
            \longitude NUMERIC(10,6) NOT NULL\
            \)" []

        run conn "CREATE TABLE IF NOT EXISTS parameter(\
            \id VARCHAR(100) NOT NULL PRIMARY KEY,\
            \name VARCHAR(100) NOT NULL,\
            \description VARCHAR(100) NOT NULL,\
            \preferredUnit VARCHAR(100) NOT NULL\
            \)"[]

        run conn "CREATE TABLE IF NOT EXISTS measurements(\
            \measurementId SERIAL PRIMARY KEY,\
            \locationId INTEGER NOT NULL,\
            \parameter INTEGER NOT NULL,\
            \value NUMERIC(10,6) NOT NULL,\
            \lastUpdated VARCHAR(100) NOT NULL,\
            \unit VARCHAR(100) NOT NULL\
            \)"[]

        commit conn
        return conn


-- | Takes a Measurement as input and returns it as a list of SqlValues - a format that can be used in a prepared statement
measurementToSqlValues :: Measurement -> [SqlValue] 
measurementToSqlValues measurement = [
     toSql $ parameter measurement,
     toSql $ value measurement,
     toSql $ lastUpdated measurement,
     toSql $ unit measurement
    ]

-- | Automation function that takes a list of Measurements and returns a list of lists of SQLValues for insertion
measurementListToSqlValues :: [Measurement] -> [[SqlValue]] 
measurementListToSqlValues = map measurementToSqlValues 

recordToSqlValues :: Record -> [SqlValue]
recordToSqlValues record = [
    toSql $ location record,
    toSql $ city record,
    toSql $ country record,
    toSql $ longitude $ coordinates record,
    toSql $ latitude $ coordinates record
    ]

-- | Takes a Parameter as input, then returns a list of SQLValues for insertion
parametersToSqlValue :: Parameter -> [SqlValue] 
parametersToSqlValue parameter = [
    toSql $ DataTypes.id parameter,
    toSql $ name parameter,
    toSql $ description parameter,
    toSql $ preferredUnit parameter
    ]

-- ^ Provides a prepared statement for inserting locations into the database.
-- ^ Takes a Connection as an input. Returns an IO Statement.
prepareInsertLocationStmt :: Connection -> IO Statement  
prepareInsertLocationStmt conn = prepare conn "INSERT INTO locations VALUES (DEFAULT, ?, ?, ?, ?, ?)"

-- | Automation function for bulk saving Records. Takes a list of Records and a Connection as input. Inserts records into the database. Returns an IO ()
saveRecords :: [Record] -> Connection -> IO () 
saveRecords records conn = do
                stmt <- prepareInsertLocationStmt conn
                executeMany stmt (map recordToSqlValues records)
                commit conn

-- | Provides a prepared statement for inserting parameters into the database. Takes a Connection as input. Returns an IO Statement
prepareInsertParameterStmt :: Connection -> IO Statement 
prepareInsertParameterStmt conn = prepare conn "INSERT INTO parameter VALUES (?, ?, ?, ?)"

-- | Automation function for bulk saving Parameters. Takes a list of Parameters and a Connection as input. Inserts parameters into the database. Returns an IO ()
saveParameters :: [Parameter] -> Connection -> IO () 
saveParameters parameters conn = do
                        stmt <- prepareInsertParameterStmt conn
                        executeMany stmt (map parametersToSqlValue parameters)
                        commit conn

-- | Provides a prepared statement for inserting measurements into the database. Takes a Connection as input. Returns as IO Statement
prepareInsertMeasurementStmt :: Connection -> IO Statement 
prepareInsertMeasurementStmt conn = prepare conn "INSERT INTO measurements VALUES (DEFAULT, ?, ?, ?, ?, ?)"

-- | Automation function for bulk saving Measurements. Takes a list of Measurements and a Connection as input. Inserts parameters into the database. Returns an IO()
saveMeasurements :: [Measurement] -> Connection -> IO ()  
saveMeasurements measurements conn = do
                        stmt <- prepareInsertMeasurementStmt conn
                        executeMany stmt (measurementListToSqlValues measurements)
                        commit conn