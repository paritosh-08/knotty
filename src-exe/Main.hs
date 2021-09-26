module Main where

import DL.Builder
import DL.Types
import Graph.Builder
import Graph.Types

{-   Doubly connected Linked List

      1
    /   \
  3 ----- 2

 -}

-- >>> take 10 $ flattenDL $ mkDL [1..3]
-- [1,2,3,1,2,3,1,2,3,1]

{-  Graph

              1
            /   \         => [1,2,3] [(1,2),(2,3),(3,1)]
          3 ----- 2

-}
-- >>> fmap (\n -> edges n) $ allNodes (mkGraphWithEdges [1,2,3] [(1,2),(2,3),(3,1)])
-- [[2,3],[1,3],[2,1]]

{- 
      1-----2-----3-----4
      |     |     |     | => [1..8] [(1,2),(2,3),(3,4),(1,5),(2,6),(3,7),(4,8),(5,6),(6,7),(7,8)]
      |     |     |     |
      5-----6-----7-----8

-}

-- >>> fmap (\n -> (n,edges n)) $ allNodes $ mkGraphWithEdges [1..8] [(1,2),(2,3),(3,4),(1,5),(2,6),(3,7),(4,8),(5,6),(6,7),(7,8)]
-- [(1,[2,5]),(2,[1,3,6]),(3,[2,4,7]),(4,[3,8]),(5,[1,6]),(6,[2,5,7]),(7,[3,6,8]),(8,[4,7])]


main :: IO ()
main = do
  print $ take 10 $ flattenDL $ mkDL [1..4]
  print $ fmap (\n -> (n,edges n)) $ allNodes $ mkGraphWithEdges [1..8] [(1,2),(2,3),(3,4),(1,5),(2,6),(3,7),(4,8),(5,6),(6,7),(7,8)]
