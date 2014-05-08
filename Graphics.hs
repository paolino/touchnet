
{-# LANGUAGE ParallelListComp #-}
-- A nifty animated fractal of a tree, superimposed on a background 
--	of three red rectangles.

import Prelude hiding (mapM, concat, foldr, concatMap, elem)
import Graphics.Gloss
import System.Random
import Graphics.Gloss.Interface.IO.Simulate
import qualified Data.IntMap as I
import qualified Data.Map as M
import Data.Foldable
import Data.Traversable

main :: IO ()
main 
 = 	simulateIO (InWindow "Zen" (800, 600) (5, 5))
                0
                50
                (zones nodes)
		(render)
                (\_ -> move)



nodes = [Node k (x,y) (0,0) (0,0)| x <- [0,40 .. 799] , y <- [0,40..599] | k <- [0..]]
index (Node k _ _ _) = k
zones = foldr f M.empty 
        where
        f n@(Node _ (x,y) _ _) = M.insertWith (++) (floor (x/20),floor (y/20)) [n]
  

data Node = Node {
        id :: Int,
        point :: (Float,Float),
        velocity :: (Float,Float),
        acceleration :: (Float,Float)
        }

type Zones = M.Map (Int,Int) [Node]

closest r (i,j) = [(x,y) | x <- [i-r .. i +r], y <- [j - r .. j +r]] 
        
render :: Zones -> IO Picture
render zs = let
        follow = let
                [(x,_)] = filter (\(_,ns) -> 100 `elem` map index ns) $ M.assocs zs
                in closest 2 x
        sq (k@(i,j),ns) = translate ((10 +) . fromIntegral $ i * 20) ((10 +) .fromIntegral $ j * 20) $ Pictures $ [color (greyN $ fromIntegral (length ns) / 10) $  rectangleSolid 20 20] ++ if k `elem` follow then [color blue $ rectangleWire 20 20] else []
        ci co (Node k (x,y) (vx,vy) (ax,ay)) = Translate x y $ (Color co (circle 5))

        in return $ Pictures $ (map sq $ M.assocs zs) ++ concatMap (\(z,xs) -> map (ci (if z `elem` follow then blue else greyN 0.5)) xs) (M.assocs zs) 
                
                

move :: Float -> Zones -> IO Zones
move t = fmap zones . mapM (cinematic t) . concat . M.elems 

cinematic _ (Node k (x,y) (vx,vy) (ax,ay)) = do
        dax <- randomRIO (-0.00001,0.00001)
        day <- randomRIO (-0.00001,0.00001)
        let 
                x' = x + vx
                y' = y + vy
                (x'',vx',ax') = if x' > 799 then 
                        (799,-vx,0)
                        else if x' < 0 then
                                (0,-vx,0)
                                else (x',vx,ax)
                (y'', vy',ay') = if y' > 599 then 
                        (599, -vy,0)
                        else if y' < 0 then
                                (0,-vy,0)
                                else (y',vy,ax)

        return $ Node
                k
                (x'',y'')
                (vx'+ ax ,vy' + ay)
                (dax + ax,day + ay)


