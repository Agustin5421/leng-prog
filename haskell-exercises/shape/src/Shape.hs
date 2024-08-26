module Shape where

data Point = Point { x::Double, y:: Double} deriving (Eq, Show)

data Circle    = Circle    Point Double deriving (Eq, Show)
data Rectangle = Rectangle Point Point deriving (Eq, Show)


-- A point from a tuple Pair
point::(Double, Double) -> Point
point (a,b) = Point a b

-- The origin
origin::Point
origin = Point 0 0

-- Rectangle from a Tuple where (x0 y0) == origin
rectangle::(Double, Double) -> Rectangle
rectangle (a,b) = Rectangle origin (Point a b)

base::Rectangle -> Double
base Rectangle (Point x0 y0) (Point x1 y1) = x1 - x0

height::Rectangle -> Double
height Rectangle (Point x0 y0) (Point x1 y1) = y1 - y0

-- Circle from radius
circle::Double -> Circle
circle a = Circle origin a

-- Class Shift

class Shift a where
   shift::a -> (Double, Double) -> a
   
instance Shift Point where
   shift (Point x y) (a,b) = Point (x+a) (y+b)
   
instance Shift Rectangle where
   shift Rectangle (Point x0 y0) (Point x1 y1) (a,b) = Rectangle (Point (x0+a) (y0+b)) (Point (x1+a) (y1+b))
   
instance Shift Circle where
   shift  Circle (Point x y) r (a,b) = Circle (Point (x+a) (y+b)) r
   
-- Define the Surface class
class Surface a where
  surface:: a -> Double

instance Surface Rectangle where
  surface r = (base r) * (height r)

instance Surface Circle where
  surface (Circle _ r) = pi * (r^2)