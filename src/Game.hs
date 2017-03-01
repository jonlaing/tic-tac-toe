module Game
    ( win
    , winner
    , fullBoard
    , gameStatus
    , gameOver
    ) where

import Board
import Data.List (transpose)

-- | Possible statuses for a game
data GameStatus = InProgress | Tie | Winner Panel
instance Show GameStatus where
  show (Winner p) = show p ++ " Wins!"
  show Tie = "It was a tie!"
  show _ = "In progress: make a move!"

-- | All the different ways one can win
data Win = Horizontal | Vertical | DiagonalLeft | DiagonalRight

-- | Checks if a line on the board has the same non-Blank panel value
-- in every index. i.e. `[X,X,X]` or `[O,O,O]`
winLine :: Panel -> Board -> Bool
winLine (Blank _) _ = False
winLine _ [] = True
winLine p (x:xs) = x == p && winLine p xs

-- | Checks if there was a win diagonally. `dist` refers to the distance
-- between the desired index to check. This function is generalized to
-- check for `DiagonalLeft` and `DiagonalRight`. With some pen and paper
-- you can figure out the requisit `dist`, or you can check the
-- `win` function
winDiag :: Int -> Panel -> Board -> [Bool]
winDiag _ _ [] = [True]
winDiag _ p [x] = (x == p) : []
winDiag dist p (x:xs) =
  (x == p) : winDiag dist p (drop dist xs)

-- | Determines whether someone won
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

-- | Returns a list of winners. Most likely, the list will at-most have
-- one element. I haven't verified that yet though.
winnerList :: Board -> [Panel]
winnerList board =
  [p
  | dir <- [Horizontal, Vertical, DiagonalLeft, DiagonalRight]
  , p <- [X, O]
  , win dir p board]

winner :: Board -> Maybe Panel
winner board =
  case winnerList board of
    [x] -> Just x
    (x:_) -> Just x
    _ -> Nothing

-- | Determines whether the board is full
fullBoard :: Board -> Bool
fullBoard board =
  not $ and $ map (\x -> canMove x board) [0..((length board) - 1)]

-- | Figures out the current game status
gameStatus :: Board -> GameStatus
gameStatus board =
  case winner board of
    Just p -> Winner p
    Nothing -> if fullBoard board
      then Tie
      else InProgress

-- | Determines whether the game is over
gameOver :: Board -> Bool
gameOver board =
  case gameStatus board of
    InProgress -> False
    _ -> True
