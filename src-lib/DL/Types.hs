module DL.Types where

data Node a = Node {
    val   :: a
  , forwN :: Node a
  , backN :: Node a
} deriving (Show)

data DL a = DL {
    f :: Node a
  , l  :: Node a
} deriving (Show)