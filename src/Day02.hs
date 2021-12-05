module Day02
  ( day02
  ) where

import Control.Monad (liftM2)
import Support

day02 = Day "02" "input/02.txt" solution1 solution2

solution1 :: Solution
solution1 = fromIntegral . uncurry (*) . foldl dive (0, 0) . map read

solution2 :: Solution
solution2 = fromIntegral . prod . foldl steer (Submarine 0 0 0) . map read

data Diving
  = Forward Int
  | Down Int
  | Up Int
  deriving (Show)

doRead :: [String] -> [(Diving, String)]
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

prod :: Submarine -> Int
prod = liftM2 (*) hPos depth
