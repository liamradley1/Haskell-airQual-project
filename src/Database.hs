{-# LANGUAGE DuplicateRecordFields #-} -- Using DuplicateRecordFields allows the records to use duplicate field labels.
{-# LANGUAGE DataKinds #-}
{-|
Module      : Database
Description : Provides functionality for initialisation of the database, conversion of data types to SqlValues, and insertion of SqlValues to the database. 

Provides functionality for initialisation of the database, conversion of data types to SqlValues, and insertion of SqlValues to the database.
Also provides functionality for querying for values in the database.
Ensure that the initialiseDB function is altered to match the PostgreSQL login details for your machine.
-}

    module Database(
        initialiseDB , saveRecords , saveParameters , saveMeasurements, getConn, dropDB, joinFromSqlValues,
        selectLocations, deleteLocations, joinParameterAndMeasurement , dbDisconnect, measurementsFromSqlValues
    ) where

import Database.HDBC
import Database.HDBC.PostgreSQL
import DataTypes

-- | An IO function that allows for the dropping of associated databases before the program starts.
dropDB :: IO ()
dropDB = do
    conn <- getConn
    run conn "DROP TABLE IF EXISTS measurements"[]
    run conn "DROP TABLE IF EXISTS parameter"[]
    run conn "DROP TABLE IF EXISTS locations"[]
    commit conn
    print "Reset."

-- | An IO function that initialises the databases if they do not already exist. Returns an IO Connection
--
-- Called twice in the main method to ensure that the data collected in the HTTP request is able to be placed.
initialiseDB :: IO Connection 
initialiseDB = do
        conn <- getConn
        run conn "CREATE TABLE IF NOT EXISTS locations (\
            \locationId SERIAL PRIMARY KEY ,\
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

-- | Returns an IO Connection to the database. Useful for various methods that require a DB connection.
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

-- | A function that converts a record into a list of SqlValues, ready for insertion.
recordToSqlValues :: Record -> [SqlValue]
recordToSqlValues record = [
    toSql $ location record,
    toSql $ city record,
    toSql $ country record,
    toSql $ longitude $ coordinates record,
    toSql $ latitude $ coordinates record
    ]

-- | Takes a Parameter as input, then returns a list of SQLValues for insertion.
parametersToSqlValue :: Parameter -> [SqlValue] 
parametersToSqlValue parameter = [
    toSql $ DataTypes.id parameter,
    toSql $ name parameter,
    toSql $ description parameter,
    toSql $ preferredUnit parameter
    ]

-- | Provides a prepared statement for inserting locations into the database.
-- Takes a Connection as an input. Returns an IO Statement.
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

-- | Takes a list of Records as an input, then extracts the location of the Record so this can be included as a foreign key in the measurements table.
extractLocations :: [Record] -> Statement -> IO ()
extractLocations [] _ = putStr ""
extractLocations (x:xs) stmt = do
    executeMany stmt (measurementListToSqlValues (measurements x) (location x))
    extractLocations xs stmt

---------------------------------------------Disconnecting From DB-------------------------------------
-- | Allows for the disconnection from a database.
dbDisconnect :: Connection -> IO ()
dbDisconnect = disconnect
-----------------------------------------------Queries--------------------------------------------------
-- | Returns a list of list of SqlValues for processing into a single list post-query.
selectLocations :: Connection -> IO [[SqlValue]]
selectLocations conn = do
    res <- quickQuery' conn "SELECT * FROM locations ORDER BY country ASC"[]
    putStrLn "Selecting locations ..."
    commit conn
    return res

-- | Converts SqlValues from the above query into a readable format.
convertSqlValues :: [SqlValue] -> [String]
convertSqlValues [] = []
convertSqlValues x = do
    [(fromSql $ x !! 1 :: String ) ++ ", " ++ (fromSql $ x !! 2 :: String ) ++ ", " ++ (fromSql $ x !! 3 :: String ) ++ ", " ++ show (fromSql $ x !! 4 :: Double ) ++ ", " ++ show (fromSql $ x !! 5 :: Double )]

-- | Prints the results of the query in a readable format.
measurementsFromSqlValues :: [[SqlValue]] -> Integer -> IO ()
measurementsFromSqlValues [] _ = putStrLn ""
measurementsFromSqlValues (x:xs) counter = do
    let val = convertSqlValues x 
    putStr (show counter ++ ". ")
    print val
    measurementsFromSqlValues xs (counter + 1)

-- | Converts SqlValues from the join query into a readable String format.
convertJoinSqlValues :: [SqlValue] -> [String]
convertJoinSqlValues [] = []
convertJoinSqlValues (x:xs) = do
    (fromSql x :: String) : convertJoinSqlValues xs 

-- | Prints out the results of the join query.
joinFromSqlValues :: (Show t, Num t) => [[SqlValue]] -> t -> IO ()
joinFromSqlValues [] _ = putStrLn ""
joinFromSqlValues (x:xs) counter = do
    let val = convertJoinSqlValues x
    putStr (show counter ++ ". ")
    print val
    joinFromSqlValues xs (counter + 1)

-- | Deletes a selected country's locations and measurements from the database.
deleteLocations :: Connection -> String -> IO ()
deleteLocations conn toDelete = do
    quickQuery' conn "DELETE FROM locations WHERE country = (?)" [toSql toDelete]
    putStrLn "Deleting countries ..."
    commit conn

-- | Returns results from the query selecting various information about the name of a parameter, the unit of measurement and the preferred scientific unit used to measure it.
joinParameterAndMeasurement :: Connection -> IO [[SqlValue]]
joinParameterAndMeasurement conn = do 
    res <- quickQuery' conn "SELECT measurements.unit, parameter.name, parameter.preferredUnit \
        \ FROM measurements INNER JOIN parameter ON \
        \ measurements.parameter = parameter.id ORDER BY measurements.measurementId ASC"[]
    putStrLn "Joining parameters and measurements tables ..."
    commit conn
    return res

