module Database
    ( initialiseDB
    ) where

import Database.HDBC
import Database.HDBC.PostgreSQL

initialiseDB :: IO Connection
initialiseDB = do
    conn <- connectPostgreSQL "host=localhost dbname = airqual_db user=postgres password =Liam"
    run conn "DROP TABLE airQual" []
    run conn "CREATE TABLE airQual(measurement_id SERIAL PRIMARY KEY, parameter VARCHAR(16) NOT NULL, date TIMESTAMPTZ, value NUMERIC(10,4) NOT NULL, unit VARCHAR(16) NOT NULL, latitude NUMERIC(10,6) NOT NULL, longitude NUMERIC(10,6) NOT NULL, country VARCHAR(16) NOT NULL, city VARCHAR(32) NOT NULL)" []
    run conn "INSERT INTO airQual VALUES (DEFAULT, 'bc', '2020-11-10T20:00:00+1:00', 0.97, 'µg/m³', 52.888422, 18.780908, 'PL', 'Ciechocinek' )" []
    -- Insertion based on the data from the following excerpt: [{"location":"Ciechocinek","parameter":"bc","date":{"utc":"2020-11-10T20:00:00Z","local":"2020-11-10T20:00:00+01:00"},"value":0.97,"unit":"µg/m³","coordinates":{"latitude":52.888422,"longitude":18.780908},"country":"PL","city":"Ciechocinek"}]}
    commit conn
    return conn