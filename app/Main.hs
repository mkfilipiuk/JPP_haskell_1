-- JPP Haskell First Assignment
-- Main module
-- Michał Filipiuk (mf385423@students.mimuw.edu.pl)

module Main where

import Lib

firstLine = "300 400 translate"
lastLine = "stroke showpage"

readCommands :: IO String
readCommands = return "lol"

main :: IO ()
main = do
         executedCommands <- readCommands
         putStrLn (firstLine ++ "\n\n" ++ executedCommands ++ "\n\n" ++ lastLine)
