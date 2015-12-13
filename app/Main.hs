module Main where

import Interval (intervalP, showInterval, joinIntervals)
import Text.Parsec.Prim (parse)
import Text.Parsec.Combinator (endBy)
import MyParser (eol)

main = do
    contents <- getContents
    case parse (endBy intervalP eol) "" contents of
         Right intervals -> do
             mapM_ (putStrLn . showInterval) $ joinIntervals intervals
         Left err -> putStrLn $ show err
