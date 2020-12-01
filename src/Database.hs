{-# LANGUAGE DeriveGeneric #-} -- Language extention
{-# LANGUAGE DuplicateRecordFields #-} -- Using DuplicateRecordFields allows the records to use duplicate field labels.
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

    module Database( -- | Provides code that allows for the initialisation, conversion from data type to SqlValue, and insertion to a database. Make sure line 19 is altered to match the PostgreSQL login details for your machine
        initialiseDB , saveRecords , saveParameters , saveMeasurements
    ) where

import Database.HDBC
import Database.HDBC.PostgreSQL
import DataTypes
import Parse


initialiseDB :: IO Connection -- ^ An IO function that initialises the databases if they do not already exist. Returns an IO Connection
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
            \id INTEGER NOT NULL,\
            \value NUMERIC(10,6) NOT NULL,\
            \lastUpdated VARCHAR(100) NOT NULL,\
            \unit VARCHAR(100) NOT NULL\
            \)"[]

        commit conn
        return conn


 
measurementToSqlValues :: Measurement -> [SqlValue] -- ^ Takes a Measurement as input and returns it as a list of SqlValues - a format that can be used in a prepared statement
measurementToSqlValues measurement = [
     toSql $ parameter measurement,
     toSql $ value measurement,
     toSql $ lastUpdated measurement,
     toSql $ unit measurement
    ]

measurementListToSqlValues :: [Measurement] -> [[SqlValue]] -- ^ Automation function that takes a list of Measurements and returns a list of lists of SQLValues for insertion
measurementListToSqlValues = map measurementToSqlValues 

recordToSqlValues :: Record -> [SqlValue]
recordToSqlValues record = [
    toSql $ location record,
    toSql $ city record,
    toSql $ country record,
    toSql $ longitude $ coordinates record,
    toSql $ latitude $ coordinates record
    ]

parametersToSqlValue :: Parameter -> [SqlValue] -- ^ Takes a Parameter as input, then returns a list of SQLValues for insertion
parametersToSqlValue parameter = [
    toSql $ DataTypes.id parameter,
    toSql $ name parameter,
    toSql $ description parameter,
    toSql $ preferredUnit parameter
    ]


prepareInsertLocationStmt :: Connection -> IO Statement -- ^ Provides a prepared statement for inserting locations into the database. Takes a Connection as an input. Returns an IO Statement 
prepareInsertLocationStmt conn = prepare conn "INSERT INTO locations VALUES (DEFAULT, ?, ?, ?, ?, ?)"

saveRecords :: [Record] -> Connection -> IO () -- ^ Takes a list of Records and a Connection as input. Inserts records into the database. Returns an IO ()
saveRecords records conn = do
                stmt <- prepareInsertLocationStmt conn
                executeMany stmt (map recordToSqlValues records)
                commit conn

prepareInsertParameterStmt :: Connection -> IO Statement -- ^ Provides a prepared statement for inserting parameters into the database. Takes a Connection as input. Returns an IO Statement
prepareInsertParameterStmt conn = prepare conn "INSERT INTO parameter VALUES (?, ?, ?, ?)"

saveParameters :: [Parameter] -> Connection -> IO () -- ^ Takes a list of Parameters and a Connection as input. Inserts parameters into the database. Returns an IO ()
saveParameters parameters conn = do
                         stmt <- prepareInsertParameterStmt conn
                         executeMany stmt (map parametersToSqlValue parameters)
                         commit conn

prepareInsertMeasurementStmt :: Connection -> IO Statement -- ^ Provides a prepared statement for inserting measurements into the database. Takes a Connection as input. Returns as IO Statement
prepareInsertMeasurementStmt conn = prepare conn "INSERT INTO measurements VALUES (DEFAULT, ?, ?, ?, ?, ?)"

saveMeasurements :: [Measurement] -> Connection -> IO () -- ^ Takes a list of Measurements and a Connection as input. Inserts parameters into the database. Returns an IO() 
saveMeasurements measurements conn = do
                            stmt <- prepareInsertMeasurementStmt conn
                            executeMany stmt (measurementListToSqlValues measurements)
                            commit conn


-------------------------------------------------previous codes--------------------------------------------------------
-- module Database
--     ( initialiseDB,
--     saveReadings
--     ) where
-- import Database.HDBC
-- import Database.HDBC.PostgreSQL
-- import Parse
--
-- initialiseDB :: IO Connection
-- initialiseDB = do
--     conn <- connectPostgreSQL "host=localhost dbname = airqual_db user=postgres password =admin"
--     run conn "DROP TABLE airQual" []
--     run conn "CREATE TABLE airQual( \
--     \ measurement_id SERIAL PRIMARY KEY, \
--     \ location VARCHAR(128) NOT NULL, \
--     \ parameter VARCHAR(16) NOT NULL, \
--     \ date TIMESTAMPTZ NOT NULL, \
--     \ value NUMERIC(10,4) NOT NULL, \
--     \ unit VARCHAR(16) NOT NULL, \
--     \ latitude NUMERIC(10,6) NOT NULL, \
--     \ longitude NUMERIC(10,6) NOT NULL, \
--     \ country VARCHAR(16) NOT NULL, \
--     \ city VARCHAR(100) NOT NULL \
--     \ )" []
--     commit conn
--     return conn
--
-- readingToSqlValues :: Reading -> [SqlValue]
-- readingToSqlValues reading = [
--         toSql $ location reading,
--         toSql $ parameter reading,
--         toSql $ utc $ date reading,
--         toSql $ value reading,
--         toSql $ unit reading,
--         toSql $ latitude $ coordinates reading,
--         toSql $ longitude $ coordinates reading,
--         toSql $ country reading,
--         toSql $ city reading
--     ]
--
-- prepareInsertReadingStmt :: Connection -> IO Statement
-- prepareInsertReadingStmt conn = prepare conn "INSERT INTO airQual VALUES (DEFAULT, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
--
-- saveReadings :: [Reading] -> Connection -> IO ()
-- saveReadings results conn = do
--     stmt <- prepareInsertReadingStmt conn
--     executeMany stmt (map readingToSqlValues results)
--     commit conn
