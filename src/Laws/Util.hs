module Laws.Util where

dropEmptyLists :: [[a]] -> [[a]]
dropEmptyLists = filter (not . null)

