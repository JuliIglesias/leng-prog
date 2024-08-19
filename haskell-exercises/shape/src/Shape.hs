module Shape where

data Point = Point { x::Double, y:: Double} deriving (Eq, Show)

-- The origin
origin::Point
origin = Point {x=0.0 , y=0.0}

-- A point from a tuple Pair
point::(Double, Double) -> Point
point (x, y) = Point {x = x , y = y}


data Rectangle = Rectangle Point Point deriving (Eq, Show)

-- Rectangle from a Tuple where (x0 y0) == origin
rectangle::(Double, Double) -> Rectangle
rectangle (x,y) = Rectangle origin (Point {x=x , y=y})

base::Rectangle -> Double
base (Rectangle p0 p1) = abs (x p1 - x p0)

height::Rectangle -> Double
height (Rectangle p0 p1) = abs (y p1 - y p0)


data Circle = Circle Point Double deriving (Eq, Show)

-- Circle from radius
circle::Double -> Circle
circle r = Circle origin r


-- Define the shift function for Point, Rectangle and Circle
-- Clase Shift
class Shift a where
   shift::a -> (Double, Double) -> a
   
instance Shift Point where
   shift (Point x y) (dx, dy) = Point (x + dx) (y + dy)
   
instance Shift Rectangle where
   shift (Rectangle origin (Point dx dy)) (dx2 , dy2) = Rectangle Point {x = dx2 + x origin, y = dy2 + y origin} Point {x = dx2+dx, y= dy2+dy}
   
instance Shift Circle where
   shift (Circle origin r) (dx, dy) = Circle (Point {x=dx + x origin, y = dy + y origin}) r


-- Define the Surface class
class Surface a where
   surface::a -> Double

instance Surface Rectangle where
  surface (Rectangle origin p1) = base (Rectangle origin p1) * height (Rectangle origin p1)

instance Surface Circle where
  surface (Circle origin r) = pi * (r ^ 2)
