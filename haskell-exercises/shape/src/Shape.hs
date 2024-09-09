module Shape where

data Point = Point { x::Double, y:: Double} deriving (Eq, Show)
data Circle    = Circle    Point Double deriving (Eq, Show)
data Rectangle = Rectangle Point Point deriving (Eq, Show)


-- A point from a tuple Pair
point::(Double, Double) -> Point
point  (a,b) = Point a b

-- The origin
origin::Point
origin = Point 0.0 0.0

-- Rectangle from a Tuple where (x0 y0) == origin
rectangle::(Double, Double) -> Rectangle
rectangle point = Rectangle origin point

base::Rectangle -> Double
base (Rectangle (Point x1 _) (Point x2 _)) =  x2 - x1

height::Rectangle -> Double
height (Rectangle (Point _ y1) (Point _ y2)) = y2 - y1

-- Circle from radius
circle::Double -> Circle
circle radius = Circle origin radius

-- Class Shift

class Shift a where
   shift::a -> (Double, Double) -> a
   
instance Shift Point where
   shift (Point x y) (x1, y1) = Point (x + x1) (y + y1)
   
instance Shift Rectangle where
   shift (Rectangle p1 p2) tuple =  Rectangle (shift p1 tuple) (shift p2 tuple)
   
instance Shift Circle where
   shift (Circle p r) (x , y) = Circle (shift p (x, y)) r
   
-- Define the Surface class
   
class Surface a where
   surface::a -> Double

instance Surface Rectangle where
    surface rectangle =  (base rectangle) * (height rectangle)

instance Surface Circle where
    surface (Circle _ r) = pi * r * r


