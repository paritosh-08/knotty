module DL.Builder where

import DL.Types

mkDL :: [a] -> DL a
mkDL [] = error "empty list"
mkDL [x] =
  let
    node = Node {
        val = x
      , forwN = node
      , backN = node
    }
  in
    DL {
        f = node
      , l = node
    }
mkDL (x:(y:ys)) =
  let
    node = Node{
        val = x
      , forwN = second
      , backN = lastN
    }
    (second, lastN) = buildNodes node node lastN y ys
  in
    DL {
        f = node
      , l = lastN
    }

buildNodes :: Node a -> Node a -> Node a -> a -> [a] -> (Node a, Node a)
buildNodes firstN justLastN lastN y [] =
  let
    node = Node {
        val = y
      , forwN = firstN
      , backN = justLastN
    }
  in (node, node)

buildNodes firstN justLastN lastN y (y2:ys) =
  let
    node = Node {
        val = y
      , forwN = nextN
      , backN = justLastN
    }
    (nextN, lastN) = buildNodes firstN node lastN y2 ys
  in (node, lastN)

flattenDL :: DL a -> [a]
flattenDL = getAllNext . f

getAllNext :: Node a -> [a]
getAllNext n = val n : getAllNext (forwN n)