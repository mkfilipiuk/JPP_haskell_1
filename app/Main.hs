-- JPP Haskell First Assignment
-- Main module
-- Michał Filipiuk (mf385423@students.mimuw.edu.pl)

module Main where

import Lib
import System.Environment
import qualified Text.Read
import Control.Monad.State.Lazy
import Control.Monad.IO.Class

firstLine = "300 400 translate"
lastLine = "stroke showpage"
errorLine = "/Courier findfont 24 scalefont setfont 0 0 moveto (Error) show"

buildingRender :: String -> State ProgramState (Either String String)
buildingRender standardInput = do
                               return (Right standardInput)

processInput :: String -> String
processInput standardInput = case (evalState (buildingRender standardInput) startState) of
                 Left _ -> errorLine
                 Right s -> s


data ProgramState = ProgramState { n :: Int
                                 , stack :: [Int]
                                 , startPoint :: Point
                                 , currentPoint :: Point
                                 , currentTransform :: Transform
                                 , picture :: Picture
                                 , isError :: Bool
                                 }

startState = ProgramState { n = 0
                          , stack = []
                          , startPoint = point (0,0)
                          , currentPoint = point (0,0)
                          , currentTransform = TranformsList []
                          , picture = Picture {pictureLines = []}
                          , isError = False
                          }

--printPicture :: IntRendering -> String


main :: IO ()
main = do
         args <- getArgs
         let argsWithOne = if null args then ["1"] else args
         case argsWithOne of
           (h:_) | ((Text.Read.readMaybe h) :: Maybe Int) /= Nothing -> do
                                                                          standardInput <- getContents
                                                                          let executedCommands = processInput standardInput
                                                                          putStrLn (firstLine ++ "\n\n" ++ executedCommands ++ "\n\n" ++ lastLine)
           otherwise -> do
                           putStrLn "Usage: ./program scale"