module Lib
  ( lvl1a
  , lvl1b
  , level2a
  , level2b
  , Diving(..)
  , Submarine(..)
  , steer
  , prod
  ) where

import Control.Monad (liftM2)

-- Level 1
countIncreases :: (Ord a, Num a) => [a] -> Int
-- countIncreases xs = length $ filter (> 0) $ zipWith (-) (tail xs) xs
countIncreases = length . filter (< 0) . (zipWith (-) <*> tail)

lvl1a :: String -> Int
lvl1a = countIncreases . map read . lines

window3 :: Num d => [d] -> [d]
-- window3 xs = zipWith3 (\a b c -> a + b + c) xs (tail xs) (tail $ tail xs)
window3 = (zipWith3 (\a b c -> a + b + c) =<< tail) =<< tail

lvl1b :: String -> Int
lvl1b = countIncreases . window3 . map read . lines

-- Level 2
data Diving
  = Forward Int
  | Down Int
  | Up Int
  deriving (Show)

doRead ("forward":m:rest) = [(Forward $ read m, unwords rest)]
doRead ("down":m:rest) = [(Down $ read m, unwords rest)]
doRead ("up":m:rest) = [(Up $ read m, unwords rest)]
doRead _ = []

instance Read Diving where
  readsPrec _ = doRead . words

dive :: (Int, Int) -> Diving -> (Int, Int)
dive (h, d) (Forward m) = (h + m, d)
dive (h, d) (Down m) = (h, d + m)
dive (h, d) (Up m) = (h, d - m)

level2a :: String -> Int
level2a = uncurry (*) . foldl dive (0, 0) . map read . lines

steer :: Submarine -> Diving -> Submarine
steer s (Forward x) = s {hPos = x + hPos s, depth = x * aim s + depth s}
steer s (Down x) = s {aim = x + aim s}
steer s (Up x) = s {aim = aim s - x}

data Submarine =
  Submarine
    { hPos :: Int
    , depth :: Int
    , aim :: Int
    }
  deriving (Show)

level2b :: String -> Int
level2b = prod . foldl steer (Submarine 0 0 0) . map read . lines

prod :: Submarine -> Int
prod = liftM2 (*) hPos depth
