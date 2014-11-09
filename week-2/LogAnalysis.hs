{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

-- |Parsing facilities
----------------------
parse :: String -> [LogMessage]
parse logs = map parseMessage (lines logs)

parseMessage :: String -> LogMessage
parseMessage line = parseMessageTokens (words line)

parseMessageTokens :: [String] -> LogMessage
parseMessageTokens ("I":t:ts)   = LogMessage Info tstamp message
  where tstamp = read t
        message = unwords ts
parseMessageTokens ("W":t:ts)   = LogMessage Warning tstamp message
  where tstamp = read t
        message = unwords ts
parseMessageTokens ("E":s:t:ts) = LogMessage (Error errorCode) tstamp message
  where errorCode = read s
        tstamp = read t
        message = unlines ts
parseMessageTokens ts           = Unknown (unwords ts)


-- |Tree-building facilities
----------------------------
build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) mt           = mt
-- that's a bit of a weird pattern match, but makes -Wall stop complaining
insert _ (Node _ (Unknown _) _) = undefined
insert lm Leaf                  = Node Leaf lm Leaf
insert lm1@(LogMessage _ t1 _) tree@(Node l lm2@(LogMessage _ t2 _) r)
  | t1 < t2                     = Node (insert lm1 l) lm2 r
  | t1 > t2                     = Node l lm2 (insert lm1 r)
  | otherwise                   = tree


-- |Tree traversal
------------------
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf         = []
inOrder (Node l m r) = inOrder l ++ [m] ++ inOrder r


-- |So, what exactly went wrong?
--------------------------------
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong logs = map showMessage severeErrors
  where severeErrors = filter isSevere orderedMsgs
        orderedMsgs = inOrder (build logs)

isSevere :: LogMessage -> Bool
isSevere (LogMessage (Error x) _ _) = x >= 50
isSevere _                          = False

showMessage :: LogMessage -> String
showMessage (LogMessage _ _ s) = s
showMessage _                  = "Undefined log message"
