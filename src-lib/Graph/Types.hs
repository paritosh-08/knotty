module Graph.Types where

data GNode a = GNode {
    val   :: a
  , edges :: [GNode a]
}

instance (Show a) => Show (GNode a) where
    show = show . val

instance (Eq a) => Eq (GNode a) where
  (==) n1 n2 = val n1 == val n2

data IGraph a = IGraph {
    allNodes :: [GNode a]
  , allEdges :: [(GNode a,GNode a)]
} deriving (Show)