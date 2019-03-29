-- JPP Haskell First Assignment
-- Main module
-- Michał Filipiuk (mf385423@students.mimuw.edu.pl)

module Main where

import Lib
import Data.Maybe
import System.Environment
import qualified Text.Read as Read
import Control.Monad.State.Lazy
import Control.Monad.IO.Class

firstLine = "300 400 translate"
lastLine = "stroke showpage"
errorLine = "/Courier findfont 24 scalefont setfont 0 0 moveto (Error) show"

data ProgramState = ProgramState { n :: Int
                                 , stack :: [R]
                                 , startPoint :: Point
                                 , startPointIsDefined :: Bool
                                 , currentPoint :: Point
                                 , currentTransform :: Transform
                                 , currentPath :: Picture
                                 , picture :: Picture
                                 , isError :: Bool
                                 }

startState scale = ProgramState { n = scale
                                , stack = []
                                , startPoint = point (0,0)
                                , startPointIsDefined = False
                                , currentPoint = point (0,0)
                                , currentTransform = TranformsList []
                                , currentPath = Picture {pictureLines = []}
                                , picture = Picture {pictureLines = []}
                                , isError = False
                                }

safeHead :: [a] -> Maybe a
safeHead l = if null l then
               Nothing
             else
               Just (head l)

pop :: State ProgramState (Maybe R)
pop = do
        s <- get
        let stackList = stack s
        let a = safeHead stackList
        if isJust a then do
          put $ s {stack = tail stackList}
          return a
        else do
          return a

push :: R -> State ProgramState ()
push i = do
           s <- get
           put $ s {stack = (i: stack s)}

add = "add"
sub = "sub"
divide = "div"
mul = "mul"
moveTo = "moveto"
lineTo = "lineto"
closePath = "closepath"
translation = "translate"
rotation = "rotate"

executeOperation :: (R -> R -> R) -> State ProgramState ()
executeOperation op = do
                        previousState <- get
                        elem2 <- pop
                        elem1 <- pop
                        s <- get
                        case (elem1, elem2) of
                          (Just element1, Just element2) -> do put $ s {stack = ((op element1 element2) : stack s)}
                          _ -> do put $ previousState {isError = True}


executeAdd = executeOperation (+)

executeSub = executeOperation (-)

executeDiv = do
               previousState <- get
               elem2 <- pop
               elem1 <- pop
               s <- get
               case (elem1, elem2) of
                 (Just element1, Just element2) | (element2 /= 0) -> do (put $ s {stack = (element1 / element2 : stack s)})
                 _ -> do put $ previousState {isError = True}

executeMul = executeOperation (*)

executeMoveTo :: State ProgramState ()
executeMoveTo = do
                  previousState <- get
                  elem2 <- pop
                  elem1 <- pop
                  s <- get
                  case (elem1, elem2) of
                   (Just element1, Just element2) -> do (put $ s {currentPoint = point (element1, element2), startPoint = (if startPointIsDefined s == False then point (element1, element2) else startPoint s), startPointIsDefined = True})
                   _ -> do put $ previousState {isError = True}

executeLineTo :: State ProgramState ()
executeLineTo = do
                  previousState <- get
                  if startPointIsDefined previousState == False then do
                    put $ previousState {isError = True}
                  else do
                    elem2 <- pop
                    elem1 <- pop
                    s <- get
                    case (elem1, elem2) of
                      (Just element1, Just element2) -> do (put $ s {currentPoint = point (element1, element2), currentPath = line (coordinates $ currentPoint s) (element1, element2) & (currentPath s)})
                      _ -> do put $ previousState {isError = True}

executeClosePath :: State ProgramState ()
executeClosePath = return () -- TODO

executeTranslate :: State ProgramState ()
executeTranslate = return () -- TODO

executeRotate :: State ProgramState ()
executeRotate = return ()-- TODO

setError :: State ProgramState ()
setError = do
             s <- get
             put $ s {isError = True}

parseInput :: [String] -> State ProgramState (Either String Picture)
parseInput [] = do
                  s <- get
                  return (Right ((currentPath s) & (picture s)))
parseInput (instruction:rest) = do
                                  let integerNumber = (Read.readMaybe instruction :: Maybe Int)
                                  case integerNumber of
                                    Just nn -> do push (toRational nn)
                                    _ -> (case () of
                                           () | instruction == add -> executeAdd
                                           () | instruction == sub -> executeSub
                                           () | instruction == divide -> executeDiv
                                           () | instruction == mul -> executeMul
                                           () | instruction == moveTo -> executeMoveTo
                                           () | instruction == lineTo -> executeLineTo
                                           () | instruction == closePath -> executeClosePath
                                           () | instruction == translation -> executeTranslate
                                           () | instruction == rotation -> executeRotate
                                           _ -> setError)
                                  s <- get
                                  if isError s then do
                                    return (Left "Error")
                                  else do
                                    parseInput rest

buildingRender :: String -> State ProgramState (Either String String)
buildingRender standardInput = do
                                 s <- get
                                 pictureEither <- parseInput $ words standardInput
                                 case pictureEither of
                                   Right picture -> return (Right (printPicture (renderScaled (n s) (reversePicture picture))))
                                   Left err -> return (Left err)

auxPrintPicture :: IntRendering -> [String] -> [String]
auxPrintPicture [] acc = acc
auxPrintPicture (((x1,y1), (x2,y2)):ls) acc = auxPrintPicture ls ((show x1 ++ " " ++ show y1 ++ " " ++ moveTo ++ " " ++ show x2 ++ " " ++ show y2 ++ " " ++ lineTo ++ "\n"):acc)

printPicture :: IntRendering -> String
printPicture intRendering = concat $ reverse $ auxPrintPicture intRendering []

processInput :: Int -> String -> String
processInput scale standardInput = case evalState (buildingRender standardInput) (startState scale) of
                                     Left _ -> errorLine
                                     Right s -> s


main :: IO ()
main = do
         args <- getArgs
         let argsWithOne = if null args then ["1"] else args
         let scale = Read.readMaybe $ head argsWithOne
         case scale of
           Just n -> do
                       standardInput <- getContents
                       let commandsToExecute = processInput n standardInput
                       putStrLn (firstLine ++ "\n\n" ++ commandsToExecute ++ "\n\n" ++ lastLine)
           _ -> putStrLn "Usage: ./program scale"