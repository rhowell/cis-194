{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

-- Exercise 1
parseMessage :: String -> LogMessage
parseMessage msg = case words msg of
                     ("W":time:rest) -> LogMessage Warning (read time :: TimeStamp) (unwords rest)
                     ("I":time:rest) -> LogMessage Info (read time :: Int) (unwords rest)
                     ("E":level:time:rest) -> LogMessage (Error (read level :: Int)) (read time :: Int) (unwords rest)
                     _ -> Unknown msg

parse :: String -> [LogMessage]
parse msg = let xs = lines msg 
            in map parseMessage xs

-- Exercise 2
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert msg Leaf = Node Leaf msg Leaf
insert msg@(LogMessage _ stmp _) (Node left curr_msg@(LogMessage _ curr_stmp _) right) | stmp < curr_stmp = Node (insert msg left) curr_msg right
                                                                                       | stmp > curr_stmp = Node left curr_msg (insert msg right)
-- Should never get here \/
insert _ tree = tree

-- Exercise 3
build :: [LogMessage] -> MessageTree
build msgs = buildBranches msgs Leaf

buildBranches :: [LogMessage] -> MessageTree -> MessageTree
buildBranches [] tree = tree
buildBranches (x:xs) tree = buildBranches xs (insert x tree)

-- Exercise 4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node Leaf msg Leaf) = [msg]
inOrder (Node left@(Node _ _ _) msg right@(Node _ _ _)) = inOrder left ++ [msg] ++ inOrder right
inOrder (Node Leaf msg right@(Node _ _ _)) = [msg] ++ inOrder right
inOrder (Node left@(Node _ _ _) msg Leaf) = inOrder left ++ [msg]

-- Exercise 5
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong [] = []
whatWentWrong msgs = map toStr (filter relevantMessage msgs)

relevantMessage :: LogMessage -> Bool
relevantMessage (LogMessage (Error sev) _ _) | sev >= 50 = True
                                             | True = False
relevantMessage _ = False

toStr :: LogMessage -> String
toStr (LogMessage _ _ s) = s
toStr _ = ""
