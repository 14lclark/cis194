{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage p
  | whatType == ["E"] = LogMessage (Error $ go 1 sep) (go 2 sep) (message 3)
  | whatType == ["I"] = LogMessage Info timeIW messIW
  | whatType == ["W"] = LogMessage Warning timeIW messIW
  | otherwise = Unknown p
  where
    go n = read . head . (take 1) . (drop n)
    sep = words p
    whatType = take 1 sep
    message n = unwords $ drop n sep
    timeIW = go 1 sep
    messIW = message 2
    
parse :: String -> [LogMessage]
parse p = [parseMessage x | x <- (lines p)]

----- BST for log messages -----

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) a = a
insert a Leaf = Node Leaf a Leaf
insert log1@(LogMessage _ t1 _) n@(Node tr1 log2@(LogMessage _ t2 _) tr2)
  | t1 < t2   = Node (insert1 tr1) log2 tr2
  | t1 > t2   = Node tr1 log2 (insert1 tr2)
  | otherwise = n
  where
    insert1 = insert log1
-- Non-exhaustive pattern because no Unknown
-- LogMessage should get through
              
build :: [LogMessage] -> MessageTree
build [] = Leaf
build (x:xs) = insert x $ build xs

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node Leaf a tr2) = a : (inOrder tr2)
inOrder (Node tr1 a Leaf) = (inOrder tr1) ++ [a]
inOrder (Node tr1 a tr2) = (inOrder tr1) ++ [a] ++ (inOrder tr2)

sortByTime :: [LogMessage] -> [LogMessage]
sortByTime = inOrder . build
                           
extractErrors :: [LogMessage] -> [LogMessage]
extractErrors [] = []
extractErrors (log1@(LogMessage (Error _) _ _):xs) =
  log1 : (extractErrors xs)
extractErrors (_:xs) = extractErrors xs

severeErrors :: [LogMessage] -> [LogMessage]
severeErrors [] = []
severeErrors (log1@(LogMessage (Error n) _ _):xs)
  | n < severeNumber = severeErrors xs
  | n >= severeNumber = log1 : (severeErrors xs)
  where
    severeNumber = 50
severeErrors (_:xs) = severeErrors xs

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong a = [msg x | x <- (sortByTime . severeErrors . extractErrors) a]
  where
    msg :: LogMessage -> String
    msg (LogMessage _ _ a) = a



-- end of assignment
