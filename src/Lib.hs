module Lib
    ( Panel
    , Board
    , newBoard
    , canMove
    , move
    , win
    , winners
    , fullBoard
    , gameStatus
    , gameOver
    ) where

import Data.List (intersperse, splitAt, transpose)
import Data.List.Split (chunksOf)

-- | The different possible states of a panel on a board
data Panel = O | X | Blank Int deriving ( Eq )
instance Show Panel where
  show (Blank n) = show (n + 1) -- ^ Just show the index, incremented by one
  show O = "O"
  show X = "X"

-- | A Board is a list of panels
type Board = [Panel]

-- / Possible statuses for a game
data GameStatus = InProgress | Tie | Winner Panel
instance Show GameStatus where
  show (Winner p) = show p ++ " Wins!"
  show Tie = "It was a tie!"
  show _ = "In progress: make a move!"

-- | All the different ways one can win
data Win = Horizontal | Vertical | DiagonalLeft | DiagonalRight

-- | Determine the length of one side of the board
dimension :: Board -> Int
dimension board = truncate $ sqrt $ fromIntegral $ length board

-- | Splits the board into lines. This is useful for determining who
-- won and also for printing the board
splitBoard :: Board -> [[Panel]]
splitBoard board = chunksOf n board
  where n = dimension board

-- / Creates a new blank board. It will throw an error if it won't
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

-- / Determines whether a move can be made at a certain location.
-- Basically, you can only move somewhere if the desired location
-- is Blank
canMove :: Int -> Board -> Bool
canMove n board = case board !! n of
  Blank _ -> True
  _ -> False

-- / Update the panel at a particular position. It will throw an error
-- if the index is out of range
update :: Int -> Panel -> Board -> Board
update n p board
  | n >= length board = error "Out of range"
  | otherwise = (take n $ fst s) ++ [p] ++ (snd s)
  where s = splitAt (n+1) board

-- / Plays a move
move :: Int -> Panel -> Board -> Board
move n p board
  | (n <= length board) && canMove pos board = update pos p board
  | otherwise = error "Illegal move"
  where pos = n - 1

-- / Checks if a line on the board has the same non-Blank panel value
-- in every index. i.e. `[X,X,X]` or `[O,O,O]`
winLine :: Panel -> Board -> Bool
winLine (Blank _) _ = False
winLine _ [] = True
winLine p (x:xs) = x == p && winLine p xs

-- / Checks if there was a win diagonally. `dist` refers to the distance
-- between the desired index to check. This function is generalized to
-- check for `DiagonalLeft` and `DiagonalRight`. With some pen and paper
-- you can figure out the requisit `dist`, or you can check the
-- `win` function
winDiag :: Int -> Panel -> Board -> [Bool]
winDiag _ _ [] = [True]
winDiag _ p [x] = (x == p) : []
winDiag dist p (x:xs) =
  (x == p) : winDiag dist p (drop dist xs)

-- / Determines whether someone won
win :: Win -> Panel -> Board -> Bool
win Horizontal player board =
  or $ map (winLine player) (splitBoard board)
win Vertical player board =
  or $ map (winLine player) (transpose $ splitBoard board)
win DiagonalLeft player board =
  and $ winDiag n player board
  where n = dimension board
win DiagonalRight player board =
  and $ winDiag n player (drop (dimension board) board)
  where n = (dimension board) - 1

-- / Returns a list of winners. Most likely, the list will at-most have
-- one element. I haven't verified that yet though.
winners :: Board -> [Panel]
winners board =
  [p
  | dir <- [Horizontal, Vertical, DiagonalLeft, DiagonalRight]
  , p <- [X, O]
  , win dir p board]

-- / Determines whether the board is full
fullBoard :: Board -> Bool
fullBoard board =
  not $ and $ map (\x -> canMove x board) [0..((length board) - 1)]

-- / Figures out the current game status
gameStatus :: Board -> GameStatus
gameStatus board
  | win = Winner $ head winner
  | fullBoard board = Tie
  | otherwise = InProgress
  where
    winner = winners board
    win = (length winner) > 0

-- / Determines whether the game is over
gameOver :: Board -> Bool
gameOver board =
  case gameStatus board of
    InProgress -> False
    _ -> True
