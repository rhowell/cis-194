{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage msg = case words msg of
                     ("W":time:rest) -> LogMessage Warning (read time :: TimeStamp) (unwords rest)
                     ("I":time:rest) -> LogMessage Info (read time :: Int) (unwords rest)
                     ("E":level:time:rest) -> LogMessage (Error (read level :: Int)) (read time :: Int) (unwords rest)
                     _ -> Unknown msg

parse :: String -> [LogMessage]
parse msg = let xs = lines msg 
            in map parseMessage xs
