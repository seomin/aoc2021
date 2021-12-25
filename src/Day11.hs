module Day11 where

import Data.Char (digitToInt)
import Data.List (findIndex)
import Data.Maybe (fromJust)
import GHC.Arr
  ( Array
  , (!)
  , (//)
  , amap
  , array
  , bounds
  , elems
  , inRange
  , indices
  , rangeSize
  )
import Support

day11 = Day "11" "input/11.txt" solution1 solution2

solution1 :: Solution
solution1 ls = sum $ steps 100 octopi
  where
    octopi = parseOctopi ls

steps :: Int -> OctopusField -> [Integer]
steps 0 _ = []
steps n octopi = flashes : steps (n - 1) octopi'
  where
    (octopi', flashes) = step octopi

newtype Octopus =
  Octopus
    { level :: Int
    }
  deriving (Show)

type Point = (Int, Int)

type OctopusField = Array Point Octopus

step :: OctopusField -> (OctopusField, Integer)
step octopi = (octopi'', flashes)
  where
    octopi' = foldl incOctopus octopi $ indices octopi
    flashes = countFlashes octopi'
    octopi'' = reset octopi'

reset :: OctopusField -> OctopusField
reset = amap $ reset' . level
  where
    reset' x =
      if x > 9
        then Octopus 0
        else Octopus x

countFlashes :: OctopusField -> Integer
countFlashes = fromIntegral . length . filter (> 9) . map level . elems

incOctopus :: OctopusField -> Point -> OctopusField
incOctopus octopi p = octopi''
  where
    octopus = octopi ! p
    l' = level octopus + 1
    octopi' = octopi // [(p, octopus {level = l'})]
    octopi'' =
      if l' == 10
        then foldl incOctopus octopi' (neighbors p octopi')
        else octopi'

neighbors :: Point -> Array Point a -> [Point]
neighbors p a =
  [ (x, y)
  | x <- map (fst p +) [-1, 0, 1]
  , y <- map (snd p +) [-1, 0, 1]
  , bounds a `inRange` (x, y)
  , (x, y) /= p
  ]

parseOctopi :: [String] -> OctopusField
parseOctopi lines =
  array
    ((0, 0), (width - 1, height - 1))
    [ ((x, y), Octopus $ digitToInt c)
    | (y, line) <- zip [0 ..] lines
    , (x, c) <- zip [0 ..] line
    ]
  where
    width = length $ head lines
    height = length lines

solution2 :: Solution
solution2 ls =
  fromIntegral $ fromJust $ findIndex ((== s) . snd) $ iterate step' (octopi, 0)
  where
    octopi = parseOctopi ls
    s = fromIntegral $ rangeSize $ bounds octopi
    step' = step . fst
