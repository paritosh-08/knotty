module Graph.Builder where

import Graph.Types

mkGraphWithEdges :: (Eq a, Show a) => [a] -> [(a,a)] -> IGraph a
mkGraphWithEdges [] _ = error "require atleast 1 node to build the graph"

mkGraphWithEdges nodes [] =
  let
    nodes' = fmap (\a ->
      GNode {
        val = a,
        edges = []
      }
      ) nodes
  in IGraph {
    allNodes = nodes',
    allEdges = []
  }

mkGraphWithEdges n e =
  let
    nodes = mkNodes n e edges
    edges = mkEdges n e nodes
  in IGraph {
    allNodes = nodes,
    allEdges = edges
  }

mkNodes :: Eq a => [a] -> [(a,a)] -> [(GNode a,GNode a)] -> [GNode a]
mkNodes n e edge = fmap (\a ->
    let
      padosi = getAllNeighbours a edge
    in GNode {
      val = a,
      edges = padosi
    }
  ) n

getAllNeighbours' :: Eq a => a -> [(GNode a, GNode a)] -> [(GNode a, GNode a)]
getAllNeighbours' v = filter (\(n1,n2) -> val n1 == v)

getAllNeighbours :: Eq a => a -> [(GNode a, GNode a)] -> [GNode a]
getAllNeighbours v edge = fmap (\(n1,n2) -> if val n1 == v then n2 else n1) (getAllNeighbours' v edge)

mkEdges :: (Eq a, Show a) => [a] -> [(a,a)] -> [GNode a] -> [(GNode a,GNode a)]
mkEdges n e nodes = foldl (\lst (a1,a2) ->
  let
    edg1 = (getNode a1 nodes, getNode a2 nodes)
    edg2 = (getNode a2 nodes, getNode a1 nodes)
    newEdges
      | edg1 `elem` lst = [edg2 | edg2 `notElem` lst]
      | edg2 `elem` lst = [edg1]
      | edg1 == edg2    = [edg1]
      | otherwise       = [edg1,edg2]
  in lst ++ newEdges
  ) [] e

getNode :: (Eq a, Show a) => a -> [GNode a] -> GNode a
getNode v nodes =
  if v `elem` fmap val nodes
    then
      head (filter (\a -> val a == v) nodes)
    else
      error (show v ++ " not found in node list " ++ show nodes)
