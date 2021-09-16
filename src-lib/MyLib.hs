module MyLib (someFunc) where

cyclic = let x = 0 : y
             y = 1 : x
         in  x

-- >>> take 20 cyclic
-- [0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1]

someFunc :: IO ()
someFunc = putStrLn "someFunc"
