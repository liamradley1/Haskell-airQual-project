module Automate (addListToDB, handleInput) where

import Parse
import Database
import Database.HDBC.PostgreSQL(Connection)
import Data.ByteString.Lazy.Char8 as L8
import Text.Read (readMaybe)
import Data.Maybe (isNothing)

addListToDB :: [IO L8.ByteString] -> Connection -> IO ()
addListToDB [] _ = print "Done!"
addListToDB (x:xs) conn = do
    json <- x
    case parse json of 
        Left _ -> addListToDB xs conn
        Right res -> do 
            print "Saving page to DB..."
            saveReadings (results res) conn
            addListToDB xs conn


handleInput :: IO Int 
-- Type-safe from using readMaybe 
handleInput = do
    print "Enter the number of entries you would like to download..."
    num <- getLine 
    if isNothing (readMaybe num :: Maybe Int) then do 
        print "Parsing error on input. Please try again."
        handleInput
    else if read num > 10000 then do -- temporary fix
        print "What could you possibly intend to do with all of that data? Please keep it to below 10000 entries."
        handleInput 
    else return $ read num
        