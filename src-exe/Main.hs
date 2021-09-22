module Main where

data Node a = Node {
    val   :: a
  , forwN :: Node a
  , backN :: Node a
} deriving (Show)

data Graph a = Graph {
    f :: Node a
  , l  :: Node a
} deriving (Show)

mkGraph :: [a] -> Graph a
mkGraph [] = error "empty list"
mkGraph [x] =
  let
    node = Node {
        val = x
      , forwN = node
      , backN = node
    }
  in
    Graph {
        f = node
      , l = node
    }
mkGraph (x:(y:ys)) =
  let
    node = Node{
        val = x
      , forwN = second
      , backN = lastN
    }
    (second, lastN) = buildNodes node node lastN y ys
  in
    Graph {
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

{- 

      1
    /   \
  3 ----- 2

 -}

-- >>> take 20 $ flattenGraph $ mkGraph [1..10]
-- [1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10]
buildNodes firstN justLastN lastN y (y2:ys) =
  let
    node = Node {
        val = y
      , forwN = nextN
      , backN = justLastN
    }
    (nextN, lastN) = buildNodes firstN node lastN y2 ys
  in (node, lastN)

flattenGraph :: Graph a -> [a]
flattenGraph = getAllNext . f

getAllNext :: Node a -> [a]
getAllNext n = val n : getAllNext (forwN n)

main :: IO ()
main = do
  print $ take 20 $ flattenGraph $ mkGraph [1..10]
