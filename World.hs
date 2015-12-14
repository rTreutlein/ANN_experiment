module World where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import qualified Data.Map.Strict as Map
import System.Random
import Debug.Trace

data Direction = UpDir | DownDir | LeftDir | RightDir deriving (Eq,Show)

type Scale = Float

data Entity = Entity
            {renderEntity :: Scale -> Location -> Picture
            ,updateEntity :: WorldMap -> Location -> [(Location,Entity)]}

instance Show Entity where
   show e = show "Entity"

type Location = (Float,Float)

type Ray = (Location,Vector)

type WorldMap = Map.Map Location Entity

type World = (Float,Float,Float,WorldMap) --Optimization switch Location/Entity (+ID)


dist (x1,y1) (x2,y2) = sqrt $ (x1-x2)^2 + (y1-y2)^2

collison :: WorldMap -> Location -> Float -> Bool
collison m p r = not $ Map.null $ Map.filterWithKey (\k _ -> r > dist k p) m

smaller a b | a < b  = a
            | a >= b = b

{-inf :: Float
inf = read "Infinity"

rayCast :: WorldMap -> Ray -> Float
rayCast m r = Map.foldrWithKey (\k e p-> smaller p $lineCollison r k) inf m

lineCollison :: Ray -> Location -> 
lineCollison (p1,d1) p2 d2
    | dx1/mag1 == dx2/mag2 && dy1/mag1 == dy2/mag2 = Nothing
   | t2 < 0 || t2>1                               = Nothing
   | t1 < 0                                       = Nothing
   | otherwise                                    = t1
       where (x1,y1)   = p1
             (dx1,dy1) = d1
         (x2,y2)   = p2
         (dx2,dy2) = d2
         mag1      = dist p1 (p1+d1)
         mag2      = dist p2 (p2+d2)
         t2        = (dx1*(y2-y1) + dy1*(x1-x2))/(dx2*dy1-dy2*dx1)
         t1        = (x2+dx2*t2-x1)/dx1-}

static :: Color -> Scale -> Entity
static c s = Entity render update where
   render scale (x,y) = translate (x*scale) (y*scale) $ color c $ circle s
   update _ (x,y)     = [((x,y),static c s)]

circlic :: Color -> Direction -> Entity
circlic c d = Entity render update where
   render scale (x,y) = translate (x*scale) (y*scale) $ color c $ circle 5
   update _ (x,y)
      | d == UpDir    = [((x,y + 1),circlic c RightDir)]
      | d == DownDir  = [((x,y-1),circlic c LeftDir)]
      | d == LeftDir  = [((x-1,y),circlic c UpDir)]
      | d == RightDir = [((x+1,y),circlic c DownDir),((x+1,y+1),circlic c UpDir)]

bush :: (RandomGen g) => Float -> Float -> Int -> g -> Entity
bush s f a g
   | a > 50    = Entity render (\_ _ -> [])
   | s > 10    = Entity render update2
   | otherwise = Entity render update
      where render scale (x,y) = translate (x*scale) (y*scale) $ color green $ circle s
            update _ (x,y)  = [((x,y),bush (s+1) (f+1) (a+1) g)]
            update2 m (x,y) =  ((x,y),bush (s-5) (f+1) (a+1) g2) : new
               where (rx,g2) = randomR (-2.0,2.0) g
                     (ry,g3) = randomR (-2.0,2.0) g2
                     col = collison m (x+rx,y+ry) 0.9
                     new = if col then [] else [((x + rx,y + ry),bush 1 0 0 g3)] --(trace (show (x+rx,y+ry)) ([((x+rx,y+ry),static red 10)]))

initWorld :: StdGen -> World
initWorld std = (10,10,20,Map.fromList
   [((1,1),bush 0 0 0 std)
   ,((1,2),bush 0 0 0 (snd $ next std))])

renderWorld :: World -> Picture
renderWorld (x,y,s,m) = translate xoff yoff $ pictures entities
   where entities = Map.foldrWithKey (\k e p-> renderEntity e s k : p) [] m
         xoff = (x/2)*(-s)
         yoff = (y/2)*(-s)

updateWorld :: ViewPort -> Float -> World -> World
updateWorld _ t (x,y,s,m) =  (x,y,s,update)
   where update = Map.foldrWithKey foldf Map.empty m
         foldf k e m2 = let es = updateEntity e m k in Map.union (Map.fromList es) m2

grid :: Float -> Float -> Float -> Picture
grid x y dist = pictures $ map line (linedown ++ lineleft)
   where lstu = [(i*dist,0) | i <- [0..x]]
         lstd = [(i*dist,y*dist) | i <- [0..x]]
         linedown  = zipWith (\a b -> [a,b]) lstu lstd

         lstl = [(0,i*dist) | i <- [0..y]]
         lstr = [(x*dist,i*dist) | i <- [0..y]]
         lineleft = zipWith (\a b -> [a,b]) lstl lstr
