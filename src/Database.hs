-- TO-DO: catch exception on local time not existing. If no local time then take UTC time

module Database
    ( initialiseDB,
    saveReadings
    ) where
import Database.HDBC
import Database.HDBC.PostgreSQL
import Parse

initialiseDB :: IO Connection
initialiseDB = do
    conn <- connectPostgreSQL "host=localhost dbname = airqual_db user=postgres password =Liam"
    run conn "DROP TABLE airQual" []
    run conn "CREATE TABLE airQual( \
    \ measurement_id SERIAL PRIMARY KEY, \
    \ location VARCHAR(128) NOT NULL, \
    \ parameter VARCHAR(16) NOT NULL, \
    \ date TIMESTAMPTZ NOT NULL, \
    \ value NUMERIC(10,4) NOT NULL, \
    \ unit VARCHAR(16) NOT NULL, \
    \ latitude NUMERIC(10,6) NOT NULL, \
    \ longitude NUMERIC(10,6) NOT NULL, \
    \ country VARCHAR(16) NOT NULL, \
    \ city VARCHAR(32) NOT NULL \
    \ )" []
    commit conn
    return conn

readingToSqlValues :: Reading -> [SqlValue]
readingToSqlValues reading = [
        toSql $ location reading,
        toSql $ parameter reading, 
        toSql $ local $ date reading, 
        toSql $ value reading, 
        toSql $ unit reading, 
        toSql $ latitude $ coordinates reading, 
        toSql $ longitude $ coordinates reading, 
        toSql $ country reading, 
        toSql $ city reading 
    ]

prepareInsertReadingStmt :: Connection -> IO Statement
prepareInsertReadingStmt conn = prepare conn "INSERT INTO airQual VALUES (DEFAULT, ?, ?, ?, ?, ?, ?, ?, ?, ?)"

saveReadings :: [Reading] -> Connection -> IO ()
saveReadings results conn = do
    stmt <- prepareInsertReadingStmt conn
    executeMany stmt (map readingToSqlValues results)
    commit conn