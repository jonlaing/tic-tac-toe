module Board
    ( Panel(..)
    , Board(..)
    , dimension
    , splitBoard
    , newBoard
    , canMove
    , move
    ) where

import Data.List (splitAt)
import Data.List.Split (chunksOf)

-- | The different possible states of a panel on a board
data Panel = O | X | Blank Int deriving ( Eq )
instance Show Panel where
  show (Blank n) = show (n + 1) -- ^ Just show the index, incremented by one
  show O = "O"
  show X = "X"

-- | A Board is a list of panels
type Board = [Panel]

-- | Determine the length of one side of the board
dimension :: Board -> Int
dimension board = truncate $ sqrt $ fromIntegral $ length board

-- | Splits the board into lines. This is useful for determining who
-- won and also for printing the board
splitBoard :: Board -> [[Panel]]
splitBoard board = chunksOf n board
  where n = dimension board

-- | Creates a new blank board. It will throw an error if it won't
-- result in a square that is playable as tic-tac-toe. A valid board
-- is a square, and has an odd number of columns
newBoard :: Int -> Board
newBoard n
  | isSquare && isOdd = map Blank [0..n-1]
  | otherwise =
    error $ "Input: " ++ show n ++ " will not make a valid Board"
  where
    isSquare = x * x == n
    isOdd = x `mod` 2 /= 0
    x = truncate $ sqrt $ fromIntegral n

-- | Determines whether a move can be made at a certain location.
-- Basically, you can only move somewhere if the desired location
-- is Blank
canMove :: Int -> Board -> Bool
canMove n board = case board !! n of
  Blank _ -> True
  _ -> False

-- | Update the panel at a particular position. It will throw an error
-- if the index is out of range
update :: Int -> Panel -> Board -> Board
update n p board
  | n >= length board = error "Out of range"
  | otherwise = (take n $ fst s) ++ [p] ++ (snd s)
  where s = splitAt (n+1) board

-- | Plays a move
move :: Int -> Panel -> Board -> Board
move n p board
  | (n <= length board) && canMove pos board = update pos p board
  | otherwise = error "Illegal move"
  where pos = n - 1
