module Database
    ( initialiseDB
    ) where

import Database.HDBC
import Database.HDBC.PostgreSQL

initialiseDB :: IO Connection
initialiseDB = do
    conn <- connectPostgreSQL "host=localhost dbname = music_database user=postgres password =Liam"
    run conn "DROP TABLE music" []
    run conn "CREATE TABLE music(name VARCHAR(50) NOT NULL)" []
    run conn "INSERT INTO music VALUES ('Test')" []
    commit conn
    return conn