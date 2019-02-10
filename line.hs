import qualified Data.Set       as S
import qualified Data.List      as L
import qualified Data.Map       as M

data Line   = Line Point Point       deriving (Show)
data Point  = Point {x::Int, y::Int} deriving (Eq, Ord)
data Color  = Color {r::Int, g::Int, b::Int}
type Pixels = M.Map Point Color

blk = Color 0 0 0
red = Color 255 0 0
blu = Color 0 0 255
grn = Color 0 255 0

p_o = Point 0 0 
p_a = Point 0 10
p_b = Point 12 4
p_c = Point 5 2

instance Show Point where
    show (Point x y) = "(" ++ show x ++ ", " ++ show y ++ ")"
-- the duality of man
instance Show Color where
    show = unwords . map show . ([r, g, b] <*>) . pure

main = do
    let oof = plotLine (Line (Point 0 0) (Point 17 11)) red
    putStrLn $ printPixels (20,20) oof

printPixels :: (Int, Int) -> Pixels -> String
printPixels (w, h) pxs = unlines . map unwords $ 
        [[show . f $ M.lookup (Point x y) pxs | x <- [0..w]] | y <- [0..h]]
    where   f Nothing  = Color 0 0 0
            f (Just c) = c 

-- that's what I call ~elegance~    (hahahahahahhah)
plotLine :: Line -> Color -> Pixels
plotLine l c = M.fromSet (\_ -> c) (S.fromList . rasterLine $ l)

-- just gives you the points a line covers, no color
rasterLine :: Line -> [Point]
rasterLine (Line p0 p1)
    | abs dx > abs dy && dx > 0 = _rLx (Line p0 p1)
    | abs dx > abs dy           = _rLx (Line p1 p0)
    | dy > 0                    = _rLy (Line p0 p1)
    | otherwise                 = _rLy (Line p1 p0)
    where   dy = (y p1) - (y p0) 
            dx = (x p1) - (x p0)

-- hell yeah ugly helper functions
--  (I could use only one by flipping the tuples somehow but ergh)
_rLx :: Line -> [Point]
_rLx (Line (Point x0 y0) (Point x1 y1)) =
    zipWith (Point) [x0..x1] ys
    where   ys = map (+y0) . map (`quot` (abs dx)) . tail $ [0, dy..]
            dy = y1 - y0
            dx = x1 - x0

_rLy :: Line -> [Point]
_rLy (Line (Point x0 y0) (Point x1 y1)) =
    zipWith (Point) xs [y0..y1]
    where   xs = map (+x0) . map (`quot` (abs dy)) . tail $ [0, dx..]
            dy = y1 - y0
            dx = x1 - x0
