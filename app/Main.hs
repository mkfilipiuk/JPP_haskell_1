-- JPP Haskell First Assignment
-- Main module
-- Michał Filipiuk (mf385423@students.mimuw.edu.pl)
module Main where

import           Control.Monad.IO.Class
import           Control.Monad.State.Lazy
import           Data.Maybe
import           Lib
import           Mon
import           System.Environment
import qualified Text.Read                as Read

firstLine = "300 400 translate"

lastLine = "stroke showpage"

errorLine = "/Courier findfont 24 scalefont setfont 0 0 moveto (Error) show"

add = "add"

sub = "sub"

divide = "div"

mul = "mul"

moveTo = "moveto"

lineTo = "lineto"

closePath = "closepath"

translation = "translate"

rotation = "rotate"

data ProgramState = ProgramState
  { n                     :: Int
  , stack                 :: [R]
  , startPoint            :: Point
  , startPointIsDefined   :: Bool
  , currentPoint          :: Point
  , currentPointIsDefined :: Bool
  , currentTransform      :: Transform
  , lengthOfCurrentPath   :: Int
  , picture               :: Picture
  , isError               :: Bool
  }

startState scale =
  ProgramState
    { n = scale
    , stack = []
    , startPoint = point (0, 0)
    , startPointIsDefined = False
    , currentPoint = point (0, 0)
    , currentPointIsDefined = False
    , currentTransform = TranformsList []
    , lengthOfCurrentPath = 0
    , picture = Picture {pictureLines = []}
    , isError = False
    }

safeHead :: [a] -> Maybe a
safeHead l =
  if null l
    then Nothing
    else Just (head l)

pop :: State ProgramState (Maybe R)
pop = do
  s <- get
  let stackList = stack s
  let a = safeHead stackList
  if isJust a
    then do
      put $ s {stack = tail stackList}
      return a
    else return a

push :: R -> State ProgramState ()
push i = do
  s <- get
  put $ s {stack = i : stack s}

executeOperation :: (R -> R -> R) -> State ProgramState ()
executeOperation op = do
  previousState <- get
  elem2 <- pop
  elem1 <- pop
  s <- get
  case (elem1, elem2) of
    (Just element1, Just element2) ->
      put $ s {stack = op element1 element2 : stack s}
    _ -> put $ previousState {isError = True}

executeAdd = executeOperation (+)

executeSub = executeOperation (-)

executeDiv = do
  previousState <- get
  elem2 <- pop
  elem1 <- pop
  s <- get
  case (elem1, elem2) of
    (Just element1, Just element2)
      | element2 /= 0 -> put $ s {stack = element1 / element2 : stack s}
    _ -> put $ previousState {isError = True}

executeMul = executeOperation (*)

executeMoveTo :: State ProgramState ()
executeMoveTo = do
  previousState <- get
  elem2 <- pop
  elem1 <- pop
  s <- get
  case (elem1, elem2) of
    (Just element1, Just element2) ->
      put $
      s
        { startPoint = transformedPoint
        , currentPoint = transformedPoint
        , startPointIsDefined = True
        , currentPointIsDefined = True
        }
      where transformedPoint =
              trpoint (currentTransform s) (point (element1, element2))
    _ -> put $ previousState {isError = True}

executeLineTo :: State ProgramState ()
executeLineTo = do
  previousState <- get
  if not (currentPointIsDefined previousState)
    then put $ previousState {isError = True}
    else do
      elem2 <- pop
      elem1 <- pop
      s <- get
      case (elem1, elem2) of
        (Just element1, Just element2) ->
          put $
          s
            { currentPoint = transformedPoint
            , picture =
                line
                  (coordinates $ currentPoint s)
                  (coordinates transformedPoint) &
                picture s
            , lengthOfCurrentPath = lengthOfCurrentPath s + 1
            }
          where transformedPoint =
                  trpoint (currentTransform s) (point (element1, element2))
        _ -> put $ previousState {isError = True}

addLineIfCurrentPath :: State ProgramState ()
addLineIfCurrentPath = do
  s <- get
  if lengthOfCurrentPath s > 0
    then put $
         s
           { picture =
               line (coordinates $ currentPoint s) (coordinates $ startPoint s) &
               picture s
           , lengthOfCurrentPath = 0
           }
    else return ()

executeClosePath :: State ProgramState ()
executeClosePath = do
  addLineIfCurrentPath
  s <- get
  put $
    s
      { currentPoint = startPoint s
      , currentTransform = TranformsList []
      }

executeTranslate :: State ProgramState ()
executeTranslate = do
  previousState <- get
  elem2 <- pop
  elem1 <- pop
  s <- get
  case (elem1, elem2) of
    (Just element1, Just element2) ->
      put $
      s
        { currentTransform =
            translate (Vec (Point (element1, element2))) >< currentTransform s
        }
    _ -> put $ previousState {isError = True}

executeRotate :: State ProgramState ()
executeRotate = do
  previousState <- get
  elem1 <- pop
  s <- get
  case elem1 of
    Just element1 ->
      put $ s {currentTransform = rotate element1 >< currentTransform s}
    _ -> put $ previousState {isError = True}

setError :: State ProgramState ()
setError = do
  s <- get
  put $ s {isError = True}

readIntWithSign :: String -> Maybe Int
readIntWithSign (s:ss) =
  if s == '+' then
    Read.readMaybe ss
  else
    Read.readMaybe (s:ss)

parseInput :: [String] -> State ProgramState (Either String Picture)
parseInput [] = Right . picture <$> get
parseInput (instruction:rest) = do
  let integerNumber = readIntWithSign instruction :: Maybe Int
  case integerNumber of
    Just nn -> push (toRational nn)
    _ ->
      case () of
        ()
          | instruction == add -> executeAdd
        ()
          | instruction == sub -> executeSub
        ()
          | instruction == divide -> executeDiv
        ()
          | instruction == mul -> executeMul
        ()
          | instruction == moveTo -> executeMoveTo
        ()
          | instruction == lineTo -> executeLineTo
        ()
          | instruction == closePath -> executeClosePath
        ()
          | instruction == translation -> executeTranslate
        ()
          | instruction == rotation -> executeRotate
        _ -> setError
  s <- get
  if isError s
    then return (Left "Error")
    else parseInput rest

buildingRender :: String -> State ProgramState (Either String String)
buildingRender standardInput = do
  s <- get
  pictureEither <- parseInput $ words standardInput
  case pictureEither of
    Right picture ->
      return
        (Right (printPicture (renderScaled (n s) (reversePicture picture))))
    Left err -> return (Left err)

auxPrintPicture :: IntRendering -> [String] -> [String]
auxPrintPicture [] acc = acc
auxPrintPicture (((x1, y1), (x2, y2)):ls) acc =
  auxPrintPicture
    ls
    ((show x1 ++
      " " ++
      show y1 ++
      " " ++ moveTo ++ " " ++ show x2 ++ " " ++ show y2 ++ " " ++ lineTo ++ "\n") :
     acc)

printPicture :: IntRendering -> String
printPicture intRendering = concat $ reverse $ auxPrintPicture intRendering []

processInput :: Int -> String -> String
processInput scale standardInput =
  case evalState (buildingRender standardInput) (startState scale) of
    Left _  -> errorLine
    Right s -> s

main :: IO ()
main = do
  args <- getArgs
  let argsWithOne =
        if null args
          then ["1"]
          else args
  let scale = readIntWithSign $ head argsWithOne
  case scale of
    Just n -> do
      standardInput <- getContents
      let commandsToExecute = processInput n standardInput
      putStrLn (firstLine ++ "\n\n" ++ commandsToExecute ++ "\n\n" ++ lastLine)
    _ -> putStrLn "Usage: ./program scale"
