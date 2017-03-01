module Main where

import Board
import Game

-- / Purely stylistic function. It's meant to put horizontal lines
-- between lines of the board when printing
horizRule :: Board -> String
horizRule board =
  (concat $ replicate n "---") ++ (concat $ replicate (n-1) "-")
  where n = dimension board

-- / Prints a line of the board
showLine :: Board -> String
showLine [] = ""
showLine [x] = " " ++ show x ++ " "
showLine (x:xs) = " " ++ show x ++ " |" ++ showLine xs

-- / Prints the whole board
showBoard :: Board -> String
showBoard [] = ""
showBoard board = unlines ls
  where
    ls = intersperse hr (map showLine (splitBoard board))
    hr = horizRule board

main :: IO ()
main = someFunc
