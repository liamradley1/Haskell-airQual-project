{-# LANGUAGE DuplicateRecordFields #-} -- Using DuplicateRecordFields allows the records to use duplicate field labels.
{-# LANGUAGE DataKinds #-}
{-|
Module      : Database
Description : Provides functionality for initialisation of the database, conversion of data types to SqlValues, and insertion of SqlValues to the database. 

Provides functionality for initialisation of the database, conversion of data types to SqlValues, and insertion of SqlValues to the database.
Ensure that the initialiseDB function is altered to match the PostgreSQL login details for your machine.
-}

    module Database(
        initialiseDB , saveRecords , saveParameters , saveMeasurements, getConn, dropDB, joinFromSqlValues,
        selectLocations, deleteLocations, joinParameterAndMeasurement , dbDisconnect, measurementsFromSqlValues
    ) where

import Database.HDBC
import Database.HDBC.PostgreSQL
import DataTypes

dropDB :: IO ()
dropDB = do
    conn <- getConn
    run conn "DROP TABLE IF EXISTS measurements"[]
    run conn "DROP TABLE IF EXISTS parameter"[]
    run conn "DROP TABLE IF EXISTS locations"[]
    commit conn
    print "Reset."

-- | An IO function that initialises the databases if they do not already exist. Returns an IO Connection
initialiseDB :: IO Connection 
initialiseDB = do
        conn <- getConn
        run conn "CREATE TABLE IF NOT EXISTS locations (\
            \locationId SERIAL NOT NULL PRIMARY KEY ,\
            \location VARCHAR(100) NOT NULL UNIQUE,\
            \city VARCHAR(100) NOT NULL,\
            \country VARCHAR(100) NOT NULL,\
            \latitude NUMERIC(10,6) NOT NULL,\
            \longitude NUMERIC(10,6) NOT NULL\
            \)" []

        run conn "CREATE TABLE IF NOT EXISTS parameter(\
            \id VARCHAR(100) PRIMARY KEY,\
            \name VARCHAR(100) NOT NULL,\
            \description VARCHAR(100) NOT NULL,\
            \preferredUnit VARCHAR(100) NOT NULL\
            \)"[]

        run conn "CREATE TABLE IF NOT EXISTS measurements(\
            \measurementId SERIAL PRIMARY KEY,\
            \location VARCHAR(100) NOT NULL,\
            \parameter VARCHAR(100) NOT NULL,\
            \value NUMERIC(10,6) NOT NULL,\
            \lastUpdated VARCHAR(100) NOT NULL,\
            \unit VARCHAR(100) NOT NULL,\
            \CONSTRAINT location_fk FOREIGN KEY(location) references locations(location) ON DELETE CASCADE,\
            \CONSTRAINT param_fk FOREIGN KEY(parameter) references parameter(id) ON DELETE NO ACTION\
            \)"[]
        commit conn
        return conn

getConn :: IO Connection
getConn = connectPostgreSQL "host=localhost dbname = airqual_db user=postgres password =admin"

-- | Takes a Measurement as input and returns it as a list of SqlValues - a format that can be used in a prepared statement
measurementToSqlValues :: Measurement -> String -> [SqlValue] 
measurementToSqlValues measurement location = [
     toSql location,
     toSql $ parameter measurement,
     toSql $ value measurement,
     toSql $ lastUpdated measurement,
     toSql $ unit measurement
    ]

-- | Automation function that takes a list of Measurements and returns a list of lists of SQLValues for insertion
measurementListToSqlValues [] _ = []
measurementListToSqlValues (x:xs) location = measurementToSqlValues x location : measurementListToSqlValues xs location
    
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
prepareInsertLocationStmt conn = prepare conn "INSERT INTO locations VALUES (DEFAULT, ?, ?, ?, ?, ?) ON CONFLICT DO NOTHING"

-- | Automation function for bulk saving Records. Takes a list of Records and a Connection as input. Inserts records into the database. Returns an IO ()
saveRecords :: [Record] -> Connection -> IO () 
saveRecords records conn = do
                stmt <- prepareInsertLocationStmt conn
                executeMany stmt (map recordToSqlValues records)
                commit conn

-- | Provides a prepared statement for inserting parameters into the database. Takes a Connection as input. Returns an IO Statement
prepareInsertParameterStmt :: Connection -> IO Statement 
prepareInsertParameterStmt conn = prepare conn "INSERT INTO parameter VALUES (?, ?, ?, ?) ON CONFLICT DO NOTHING"

-- | Automation function for bulk saving Parameters. Takes a list of Parameters and a Connection as input. Inserts parameters into the database. Returns an IO ()
saveParameters :: [Parameter] -> Connection -> IO () 
saveParameters parameters conn = do
                        stmt <- prepareInsertParameterStmt conn
                        executeMany stmt (map parametersToSqlValue parameters)
                        commit conn

-- | Provides a prepared statement for inserting measurements into the database. Takes a Connection as input. Returns as IO Statement
prepareInsertMeasurementStmt :: Connection -> IO Statement 
prepareInsertMeasurementStmt conn = prepare conn "INSERT INTO measurements VALUES (DEFAULT, ?, ?, ?, ?, ?) ON CONFLICT DO NOTHING"

-- | Automation function for bulk saving Measurements. Takes a list of Measurements and a Connection as input. Inserts parameters into the database. Returns an IO()
saveMeasurements :: [Record] -> Connection -> IO ()  
saveMeasurements record conn = do
                        stmt <- prepareInsertMeasurementStmt conn
                        extractLocations record stmt
                        commit conn

extractLocations :: [Record] -> Statement -> IO ()
extractLocations [] _ = putStr ""
extractLocations (x:xs) stmt = do
    executeMany stmt (measurementListToSqlValues (measurements x) (location x))
    extractLocations xs stmt

---------------------------------------------Disconnecting From DB-------------------------------------

dbDisconnect :: Connection -> IO ()
dbDisconnect = disconnect
-----------------------------------------------Queries--------------------------------------------------

selectLocations :: Connection -> IO [[SqlValue]]
selectLocations conn = do
    res <- quickQuery' conn "SELECT * FROM locations ORDER BY country ASC"[]
    putStrLn "Selecting locations ..."
    commit conn
    return res

convertSqlValues :: [SqlValue] -> [String]
convertSqlValues [] = []
convertSqlValues x = do
    [(fromSql $ x !! 1 :: String ) ++ ", " ++ (fromSql $ x !! 2 :: String ) ++ ", " ++ (fromSql $ x !! 3 :: String ) ++ ", " ++ show (fromSql $ x !! 4 :: Double ) ++ ", " ++ show (fromSql $ x !! 5 :: Double )]

convertJoinSqlValues [] = []
convertJoinSqlValues (x:xs) = do
    (fromSql x :: String) : convertJoinSqlValues xs 
measurementsFromSqlValues :: [[SqlValue]] -> Integer -> IO ()
measurementsFromSqlValues [] _ = putStrLn ""
measurementsFromSqlValues (x:xs) counter = do
    let val = convertSqlValues x 
    putStr (show counter ++ ". ")
    print val
    measurementsFromSqlValues xs (counter + 1)

joinFromSqlValues [] _ = putStrLn ""
joinFromSqlValues (x:xs) counter = do
    let val = convertJoinSqlValues x
    putStr (show counter ++ ". ")
    print val
    joinFromSqlValues xs (counter + 1)

deleteLocations :: Connection -> String -> IO [[SqlValue]]
deleteLocations conn toDelete = do
    res <- quickQuery' conn "DELETE FROM locations WHERE country = (?)"[toSql toDelete]
    putStrLn "Deleting countries ..."
    commit conn
    return res

joinParameterAndMeasurement :: Connection -> IO [[SqlValue]]
joinParameterAndMeasurement conn = do 
    res <- quickQuery' conn "SELECT measurements.measurementId, measurements.unit, parameter.name, parameter.preferredUnit \
        \ FROM measurements INNER JOIN parameter ON \
        \ measurements.parameter = parameter.id ORDER BY measurements.measurementId ASC"[]
    putStrLn "Joining parameters and measurements tables ..."
    commit conn
    return res

