--TO-DO: catch exception on local time not existing. If no local time then take UTC time
--  module Database
--      ( initialiseDB,
--        saveRecords
--      ) where
    module Database(
        initialiseDB , parametersToSqlValue, recordToSqlValues
    ) where

import Database.HDBC
import Database.HDBC.PostgreSQL
import DataTypes
-- import DataTypes ( Parameter(id, name, description, preferredUnit), Record(location, city, country, measurements, coordinates))

initialiseDB :: IO Connection
initialiseDB =
    do
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

-- This  method is intended  to convert measurement variables to sqlValue first
-- measurementToSqlValues :: Measurement -> [SqlValue]
-- measurementToSqlValues measurement = [
--     toSql $ parameter measurement,
--     toSql $ value measurement,
--     toSql $ lastUpdated measurement,
--     toSql $ unit measurement
--   ]

recordToSqlValues :: Record -> [SqlValue]
recordToSqlValues record = [
    toSql $ location record,
    toSql $ city record,
    toSql $ country record,
    toSql $ longitude $ coordinates record,
    toSql $ latitude $ coordinates record
    --measurementToSqlValues        -- calling the method above to give us a list of sqlValue measurements
    ]

parametersToSqlValue :: Parameter -> [SqlValue]
parametersToSqlValue parameter = [
    toSql $  DataTypes.id parameter,
    toSql $  name  parameter,
    toSql $  description parameter,
    toSql $  preferredUnit parameter
    ]


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
