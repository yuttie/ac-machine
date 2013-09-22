module Main where

import Data.AhoCorasick


main :: IO ()
main = do
    let acm = construct ["he", "she", "his", "hers"]
    print $ run acm "ushers"
    let acm' = constructWithValues [("he", 1.2), ("she", 3.4), ("his", 5.6), ("hers", 7.8)]
    print $ run acm' "ushers"
