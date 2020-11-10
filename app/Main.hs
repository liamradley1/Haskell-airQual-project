module Main where

import Lib
import Database
main :: IO ()
main = do
    print "Initialising..."
    initialiseDB
    print "Done!"