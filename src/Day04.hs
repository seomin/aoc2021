{-# LANGUAGE TupleSections #-}

module Day04
  ( day04
  , parseBoards
  , winners
  ) where

import Data.List (delete, find)
import Data.List.Split
import Debug.Trace (trace, traceStack)
import Support
import Text.Printf (printf)

day04 = Day "04" "input/04.txt" solution1 solution2

solution1 :: Solution
solution1 (numberString:boardString) = fromIntegral $ unmarked * winningNumber
  where
    numbers = map read $ splitOn "," numberString
    boards = parseBoards boardString
    (winner, winningNumber) = head $ winners numbers boards
    unmarked = sum . map fst . filter (not . snd) $ winner
solution1 _ = error "unreachable"

parseBoards :: [String] -> [Board]
parseBoards [] = []
parseBoards ("":bs) = parseBoards bs
parseBoards (a:b:c:d:e:rest) = board : parseBoards rest
  where
    board =
      parseLine a ++ parseLine b ++ parseLine c ++ parseLine d ++ parseLine e
    parseLine :: String -> [(Int, Bool)]
    parseLine = map ((, False) . read) . words
parseBoards _ = error "unreachable"

type Board = [(Int, Bool)]

winners :: [Int] -> [Board] -> [(Board, Int)]
winners [] _ = []
winners (call:numbers) boards =
  case find winner nextBoards of
    Just w ->
      traceStack
        (printf "found winner %s with number %d" (show w) call)
        (w, call) :
      winners numbers (delete w nextBoards)
    Nothing -> winners numbers nextBoards
  where
    nextBoards = map (mark call) boards

winner :: Board -> Bool
winner b = winningRow b || winningCol b

winningCol :: Board -> Bool
winningCol board =
  or [and [snd $ board !! (i + j) | j <- [0,5 .. 20]] | i <- [0 .. 4]]

winningRow :: Board -> Bool
winningRow [] = False
winningRow board = (all snd . take 5) board || winningRow (drop 5 board)

mark :: Int -> Board -> Board
mark number [] = []
mark number ((n, m):rest)
  | number == n = (n, True) : rest
  | otherwise = (n, m) : mark number rest

solution2 :: Solution
solution2 (numberString:boardString) = fromIntegral $ unmarked * winningNumber
  where
    numbers = map read $ splitOn "," numberString
    boards = parseBoards boardString
    (winner, winningNumber) =
      last $ trace ("winners length " ++ show (length allWinners)) allWinners
    allWinners = winners numbers boards
    unmarked = sum . map fst . filter (not . snd) $ winner
solution2 _ = error "unreachable"
