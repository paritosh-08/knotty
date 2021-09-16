module Main where

data GraphN a = GraphNode {
  val :: a,
  edges :: [Edge a]
}

instance Eq a => Eq (GraphN a) where
  (==) g1 g2 = val g1 == val g2
  (/=) g1 g2 = val g1 /= val g2

instance Show a => Show (GraphN a) where
  show g = show (val g)

instance Show a => Show (Edge a) where
  show e = show (from e) ++ "------" ++ show (to e)

data Edge a = Edge {
  from :: GraphN a,
  to   :: GraphN a
}

t1 :: GraphN Int
t1 = GraphNode{
  val = 1,
  edges = [e12, e15]
}
t2 :: GraphN Int
t2 = GraphNode{
  val = 2,
  edges = [e21, e23, e26]
}
t3 :: GraphN Int
t3 = GraphNode{
  val = 3,
  edges = [e32, e34, e37]
}
t4 :: GraphN Int
t4 = GraphNode{
  val = 4,
  edges = [e43, e48]
}
t5 :: GraphN Int
t5 = GraphNode{
  val = 5,
  edges = [e51, e56]
}
t6 :: GraphN Int
t6 = GraphNode{
  val = 6,
  edges = [e62, e65, e67]
}
t7 :: GraphN Int
t7 = GraphNode{
  val = 7,
  edges = [e73, e76, e78]
}
t8 :: GraphN Int
t8 = GraphNode{
  val = 8,
  edges = [e84, e78]
}

e12 :: Edge Int
e12 = Edge{
  from = t1,
  to   = t2
}
e23 :: Edge Int
e23 = Edge{
  from = t2,
  to   = t3
}
e34 :: Edge Int
e34 = Edge{
  from = t3,
  to   = t4
}
e15 :: Edge Int
e15 = Edge{
  from = t1,
  to   = t5
}
e26 :: Edge Int
e26 = Edge{
  from = t2,
  to   = t6
}
e37 :: Edge Int
e37 = Edge{
  from = t3,
  to   = t7
}
e48 :: Edge Int
e48 = Edge{
  from = t4,
  to   = t8
}
e56 :: Edge Int
e56 = Edge{
  from = t5,
  to   = t6
}
e67 :: Edge Int
e67 = Edge{
  from = t6,
  to   = t7
}
e78 :: Edge Int
e78 = Edge{
  from = t7,
  to   = t8
}

e21 :: Edge Int
e21 = Edge{
  from = t2,
  to   = t1
}
e32 :: Edge Int
e32 = Edge{
  from = t3,
  to   = t2
}
e43 :: Edge Int
e43 = Edge{
  from = t4,
  to   = t3
}
e51 :: Edge Int
e51 = Edge{
  from = t5,
  to   = t1
}
e62 :: Edge Int
e62 = Edge{
  from = t6,
  to   = t2
}
e73 :: Edge Int
e73 = Edge{
  from = t7,
  to   = t3
}
e84 :: Edge Int
e84 = Edge{
  from = t8,
  to   = t4
}
e65 :: Edge Int
e65 = Edge{
  from = t6,
  to   = t5
}
e76 :: Edge Int
e76 = Edge{
  from = t7,
  to   = t6
}
e87 :: Edge Int
e87 = Edge{
  from = t8,
  to   = t7
}

{- 

  1-----2-----3-----4
  |     |     |     |
  |     |     |     |
  5-----6-----7-----8

 -}

-- >>> printG t1

printG :: (Show a, Eq a) => GraphN a -> [a] -> IO ()
printG g [] = do
  putStr $ show (val g)
  putStr "-----"
  mapM_ (\a ->
    printG (to a) [val g]
    ) (edges g)

printG g xs = do
  if val g `elem` xs
    then putStr ""
    else do
      putStr $ show (val g)
      putStr "-----"
      mapM_ (\a ->
        printG (to a) (xs ++ [val g])
        ) (edges g)


main :: IO ()
main = do
  printG t1 []
  putStr "\n"
