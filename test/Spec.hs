import Lib

-- składanie translacji
testCompTrans :: R2 -> R2 -> R2 -> Bool
testCompTrans v1 v2 p = trpoint (translate (vec v1) >< translate (vec v2)) (point p)
                        == trpoint (translate (vec v1 >< vec v2)) (point p)

-- składanie rotacji
testCompRot :: R -> R -> R2 -> Bool
testCompRot r1 r2 p = trpoint (rotate r1 >< rotate r2) (point p)
                        == trpoint (rotate (r1 + r2)) (point p)

-- łączność składania rotacji
testAssRot :: R -> R -> R -> R2 -> Bool
testAssRot a b c p = trpoint (rotate a >< (rotate b >< rotate c)) (point p)
                        == trpoint ((rotate a >< rotate b) >< rotate c) (point p)

-- rotacja o pełny obrót jest identycznością
testFullCircleRot :: R2 -> Bool
testFullCircleRot p = trpoint (rotate fullCircle) (point p) == (point p)

main :: IO ()
main = do
         putStrLn "Running tests"

