import qualified Data.Set        as S
import qualified Data.List       as L
import qualified Data.Map.Strict as M

data Line   = Line Point Point deriving (Show)
data Point  = Point {getX::Int, getY::Int} deriving (Eq, Ord)
data Color  = Color {r::Int, g::Int, b::Int}

type Pixels = M.Map Point Color
-- if I want to do more cool stuff this is going to have to be a newtype

blk = Color 0 0 0
red = Color 255 0 0
blu = Color 0 0 255
grn = Color 0 255 0

-- Prelude.pi is a Floating and not a Fractional and that's stupid
_pi = 3.14159

instance Show Point where
    show (Point x y) = "(" ++ show x ++ ", " ++ show y ++ ")"
-- the duality of man
instance Show Color where
    show = unwords . map show . ([r, g, b] <*>) . pure

ppmHeader :: (Int, Int) -> String
ppmHeader (w, h) = "P3 " ++ show w ++ " " ++ show h ++ " 255\n"

-- all (non-permuted) pairs of a list
allPairs :: [a] -> [(a, a)]
allPairs []     = []
allPairs (_:[]) = []
allPairs (x:xs) = map ((,) x) xs ++ allPairs xs
-- look at me actually writing comments

main = do
    let points  = polygon 21 255 (Point 300 300)
        lines   = [Line p0 p1 | (p0, p1) <- allPairs points]
        drawing = [plotLine (Line p0 p1)
                            (Color  ((getX p0 + getX p1) `div` 4) 0
                                    ((getY p0 + getY p1) `div` 4))
                    | (Line p0 p1) <- lines]
    writeFile "out.ppm" (printPixels (600, 600) (mconcat drawing))

-- floating point math :'(
polygon :: (Integral a) => a -> a -> Point -> [Point]
polygon s r (Point x y) =
    [Point  (x + round (fromIntegral r * sin ((fromIntegral i) * th)))
            (y + round (fromIntegral r * cos ((fromIntegral i) * th)))
        | i <- [0..s-1]]
    where th = (2 * _pi) / (fromIntegral s)
-- cmon haskell I'm just trying to multiply a sine why do you have to make
--  the typing so hard

-- takes bounds and a screen and puts in ppm format
printPixels :: (Int, Int) -> Pixels -> String
printPixels (w, h) pxs =
    ppmHeader (w, h)
    ++ (unlines . map unwords $ [[show . f $ M.lookup (Point x y) pxs
                | x <- [0..w-1]] | y <- [0..h-1]])
    where   f Nothing  = Color 0 0 0
            f (Just c) = c 

-- that's what I call ~elegance~    (hahahahahahhah)
plotLine :: Line -> Color -> Pixels
plotLine l c = M.fromSet (\_ -> c) (S.fromList . rasterLine $ l)

-- just gives you the points a line covers, no color
rasterLine :: Line -> [Point]
rasterLine (Line p0 p1)
    | dy == 0 && dx == 0        = []
    | abs dx > abs dy && dx > 0 = _rLx (Line p0 p1)
    | abs dx > abs dy           = _rLx (Line p1 p0)
    | dy > 0                    = _rLy (Line p0 p1)
    | otherwise                 = _rLy (Line p1 p0)
    where   dy = (getY p1) - (getY p0) 
            dx = (getX p1) - (getX p0)

-- hell yeah ugly helper functions
--  (I could use only one by flipping the tuples somehow but ergh)
_rLx :: Line -> [Point]
_rLx (Line (Point x0 y0) (Point x1 y1)) =
    zipWith (Point) [x0..x1] ys
    where   ys = map (+y0) . map (`quot` (2 * abs dx)) $ [dy, 3*dy..]
            dy = y1 - y0
            dx = x1 - x0

_rLy :: Line -> [Point]
_rLy (Line (Point x0 y0) (Point x1 y1)) =
    zipWith (Point) xs [y0..y1]
    where   xs = map (+x0) . map (`quot` (2 * abs dy)) $ [dx, 3*dx..]
            dy = y1 - y0
            dx = x1 - x0

-- persuit curve
--    let lines = [(Line (Point x 0) (Point 0 (255-x))) | x <- [0,5..255]]
--        drawing = [plotLine (Line p0 p1) (Color ((getX p0)+(getX p1))
--                                                0
--                                                ((getY p0)+(getY p1)))
--                                                
--                    | (Line p0 p1) <- lines]
