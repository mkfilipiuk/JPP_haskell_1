-- JPP Haskell First Assignment
-- Lib module
-- Michał Filipiuk (mf385423@students.mimuw.edu.pl)a

module Lib where

import Data.Ratio
import Mon


type R = Rational
type R2 = (R,R)


zeroR = (0 % 1)
zeroR2 = (zeroR, zeroR)
pointZero = Point {coordinates = zeroR2}


data Point = Point {coordinates :: R2}

instance Eq Point where
  Point {coordinates = (a,b)} == Point {coordinates = (x,y)} = a == x && b == y
  x /= y = not (x == y)

instance Show Point where
  show Point {coordinates = (a,b)} = "(" ++ (show a) ++ " , " ++ (show b) ++ ")"

point :: R2 -> Point
point r2 = Point {coordinates = r2}


data Vec = Vec { direction :: Point }

instance Eq Vec where
  Vec {direction = a} == Vec {direction = x} = a == x
  x /= y = not (x == y)

instance Show Vec where
  show (Vec {direction = a}) = concat ["(", (show a) , ")"]

instance Mon Vec where
  m1 = Vec {direction = pointZero}
  (><) Vec {direction = Point {coordinates = (a,b)}} Vec {direction = Point {coordinates = (x,y)}} = Vec {direction = Point {coordinates = (a+x,b+y)}}

vec :: R2 -> Vec
vec r2 = Vec { direction = (point r2) }


type Line = (Point, Point)

data Picture = Picture {pictureLines :: [Line]}

line :: (R,R) -> (R,R) -> Picture
line rs1 rs2 = Picture {pictureLines = [((point rs1),(point rs2))]}

rectangle :: R -> R -> Picture
rectangle r1 r2 = Picture {pictureLines = [((point zeroR2),(point (zeroR, r1)))
                                          ,((point zeroR2),(point (zeroR, r2)))
                                          ,((point (zeroR, r1)), (point (r1,r2)))
                                          ,((point (zeroR, r2)), (point (r1,r2)))
                                          ]}

(&) :: Picture -> Picture -> Picture
(&) Picture {pictureLines = l1} Picture {pictureLines = l2} = Picture {pictureLines = l1 ++ l2}


type IntLine = ((Int,Int), (Int,Int))
type IntRendering = [IntLine]

renderScaled :: Integer -> Picture -> IntRendering
renderScaled i Picture {pictureLines = l} = map (scaleLine i) l

scaleLine :: Integer -> Line -> IntLine
scaleLine i (p1, p2) = (roundPoint (scalePoint i p1), roundPoint (scalePoint i p2))

scalePoint :: Integer -> Point -> Point
scalePoint i Point {coordinates = (a,b)} = Point {coordinates = ((i % 1)*a, (i % 1)*b)}

roundPoint :: Point -> (Int,Int)
roundPoint Point {coordinates = (a,b)} = (round a, round b)


data AuxTransform =  VectorTransform Vec | RotationTransform R deriving (Eq, Show)

bhaskaraSineApproximation :: R -> R
bhaskaraSineApproximation x = ((4*x*(180 - x)) / (40500 - x*(180 - x)))

rotateSine :: R -> R
rotateSine d = if d < (180 % 1) then
                 bhaskaraSineApproximation d
               else
                 -(bhaskaraSineApproximation (d-180))

rotateCosine :: R -> R
rotateCosine d = rotateSine (normalizeDegree (d+90))

applyAuxTransformToPoint :: AuxTransform -> Point -> Point
applyAuxTransformToPoint (VectorTransform translation) p = direction (Vec { direction = p } >< translation)
applyAuxTransformToPoint (RotationTransform rotation) (Point {coordinates = (r1,r2)}) = Point {coordinates = (r1*c - r2*s, r1*s + r2*c)}
                                                                    where s = rotateSine rotation
                                                                          c = rotateCosine rotation

data Transform = TranformsList [AuxTransform] deriving (Eq, Show)

translate :: Vec -> Transform
translate v = TranformsList [VectorTransform v]

normalizeDegree :: R -> R
normalizeDegree r = if r > 0 then
                        r - ((quot (numerator r) ((denominator r)* fullCircleInDegrees))*fullCircleInDegrees % 1)
                      else
                        r - (((quot (numerator r) ((denominator r)* fullCircleInDegrees))*fullCircleInDegrees - fullCircleInDegrees )% 1)

rotate :: R -> Transform
rotate r = TranformsList [RotationTransform (normalizeDegree r)]

fullCircleInDegrees :: Integer
fullCircleInDegrees = 360

fullCircle :: R
fullCircle = (fullCircleInDegrees % 1)

instance Mon Transform where
  m1 = TranformsList []
  (><) t1 t2 = mergeTransformations t1 t2

mergeTransformations :: Transform -> Transform -> Transform
mergeTransformations (TranformsList []) t2 = t2
mergeTransformations t1 (TranformsList []) = t1
mergeTransformations (TranformsList t1) (TranformsList t2) = auxMergeTransformations (TranformsList (reverse (t1 ++ t2))) []

auxMergeTransformations :: Transform -> [AuxTransform] -> Transform
auxMergeTransformations (TranformsList []) acc = TranformsList acc
auxMergeTransformations (TranformsList (t:ts)) [] = auxMergeTransformations (TranformsList ts) [t]
auxMergeTransformations (TranformsList (t:ts)) (acc:accs) = case (t,acc) of
                                                              (VectorTransform v1, VectorTransform v2) -> auxMergeTransformations (TranformsList ts) (VectorTransform (v1 >< v2):accs)
                                                              (RotationTransform r1, RotationTransform r2) -> auxMergeTransformations (TranformsList ts) (RotationTransform (normalizeDegree (r1 + r2)):accs)
                                                              (VectorTransform v1, RotationTransform r2) -> auxMergeTransformations (TranformsList ts) ([VectorTransform v1, RotationTransform r2] ++ accs)
                                                              (RotationTransform r1, VectorTransform v2) -> auxMergeTransformations (TranformsList ts) ([RotationTransform r1, VectorTransform v2] ++ accs)

trpoint :: Transform -> Point -> Point
trpoint (TranformsList auxTransforms) p = foldl (\y -> \x -> applyAuxTransformToPoint x y) p auxTransforms

trvec :: Transform -> Vec -> Vec
trvec (TranformsList auxTransforms) Vec { direction = p} = Vec { direction = (foldl (\y -> \x -> applyAuxTransformToPoint x y) p auxTransforms)}

trline :: Transform -> Line -> Line
trline t (p1,p2) = (trpoint t p1, trpoint t p2)

transform :: Transform -> Picture -> Picture
transform t Picture {pictureLines = pl} = Picture {pictureLines = (map (trline t) pl)}

--TranformsList [VectorTransform (((-1941598036941) % 1969679790068) -> ((-3492762117037) % 6398678210452))]
--TranformsList [RotationTransform (1597065196210031 % 4442737490389)]
--TranformsList [VectorTransform (((-387920482565) % 1678482931093) -> (775939336714 % 2320104954661))]
