
{-# LANGUAGE ParallelListComp #-}
-- A nifty animated fractal of a tree, superimposed on a background 
--	of three red rectangles.

import Prelude hiding (mapM, concat, foldr, concatMap, elem, sum)
import Graphics.Gloss
import System.Random
import Graphics.Gloss.Interface.IO.Simulate
import qualified Data.IntMap.Strict as I
import qualified Data.Map.Strict as M
import Data.Foldable
import Data.Traversable
import qualified Data.Set as S
import System.Time
import Control.DeepSeq

main :: IO ()
main 
 = 	simulateIO (InWindow "Zen" (800, 600) (5, 5))
                0
                50
                (0,zones nodes)
		(render)
                (\_ -> move)


nodes :: [Node]
nodes = [Node k (x,y) (0,0) (0,0) $ M.empty| x <- [0,40 .. 799] , y <- [0,40..599] | k <- [0..240]]

index (Node k _ _ _ _) = k
listeners (Node _ _ _ _ r) = r

zones :: [Node] -> (Locate,Zones)
zones = foldr f (I.empty,M.empty) 
        where
        f n@(Node i (x,y) _ _ _) (ls,zs) = (I.insert i (x,y) ls, M.insertWith (++) (floor (x/20),floor (y/20)) [n] zs)
  

data Node = Node {
        id :: Int,
        point :: (Float,Float),
        velocity :: (Float,Float),
        acceleration :: (Float,Float),
        receivers :: M.Map Int Int
        }

type Zones = M.Map (Int,Int) [Node]
type Locate = I.IntMap (Float,Float)

closest r (i,j) = [(x,y) | x <- [i-r .. i +r], y <- [j - r .. j +r]] 
        
render :: (Int,(Locate,Zones)) -> IO Picture
render (h,(loc,zs)) = let
        (follow,l,vs) = let
                [(k,rs)] = filter (\(_,ns) -> h `elem` map index ns) $ M.assocs zs
                Just (Node _ l _ _ vs)  = find ((== h) . index) rs
                in (closest 2 k,l,vs)
        sq (k@(i,j),ns) = translate ((10 +) . fromIntegral $ i * 20) ((10 +) .fromIntegral $ j * 20) $ Pictures $ [color (greyN $ fromIntegral (length ns) / 10) $  rectangleSolid 20 20] 
        
        ci  (Node k (x,y) (vx,vy) (ax,ay) _) = Translate x y $ (Color (greyN 0.5) (circle 5))
        cv (Node k (x,y) (vx,vy) (ax,ay) _ ) = color red $ line [l,(x,y)]
        cl (Node k (x,y) (vx,vy) (ax,ay) _) = color blue $ line [l,(x,y)]
        drawNode z n = Pictures $ [ci n] ++ if z `elem` follow then [cv n] else [] -- ++ if index n `M.member` vs then [cl n] else []
        
        in return $ Pictures $ (map sq $ M.assocs zs) ++ concatMap (\(z,xs) -> map (drawNode z) xs) (M.assocs zs) 
                
                

move :: Float -> (Int,(Locate,Zones)) -> IO (Int,(Locate,Zones))
move t (h,(ls,zs)) = do 
        (ls,zs') <- fmap zones . fmap concat . mapM (cinematica t zs ls) . M.elems $ zs
        return (0,(ls,zs'))

cinematica t zs l ns = do
        now <- millis
        mapM (cinematic (now `mod` 1000 == 0) t zs l) ns
        

weight k (x,y) (Node i p (x1,y1) a rs) = Node i p (k * x + (1 - k) * x1, k*y + (1 - k) * y1) a rs

limit z u x = max z (min u x)

collect :: Zones -> Int -> (Float,Float) -> [Int]
collect zs r (x,y) = let
        k = (floor (x/20),floor (y / 20))
        in map index . concatMap (\k' -> M.findWithDefault []  k' zs) $ closest r k 

millis :: IO Integer
millis = getClockTime >>= \(TOD x y) -> return $ x * 1000 + (y `div` 1000000000)

cinematic d _ zs l (Node k (x,y) (vx,vy) (ax,ay) rs) = do
        
        dax <- randomRIO (-0.0001,0.0001)
        day <- randomRIO (-0.0001,0.0001)
        return $  M.unionWith (+) M.empty $ rs 
                


