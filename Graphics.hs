
{-# LANGUAGE ParallelListComp #-}
-- A nifty animated fractal of a tree, superimposed on a background 
--	of three red rectangles.

import Prelude hiding (mapM, concat, foldr, concatMap, elem)
import Graphics.Gloss
import System.Random
import Graphics.Gloss.Interface.IO.Simulate
import qualified Data.IntMap.Strict as I
import qualified Data.Map.Strict as M
import Data.Foldable
import Data.Traversable

main :: IO ()
main 
 = 	simulateIO (InWindow "Zen" (800, 600) (5, 5))
                0
                50
                (0,zones nodes)
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
        
render :: (Int,Zones) -> IO Picture
render (h,zs) = let
        (follow,l) = let
                [(k,rs)] = filter (\(_,ns) -> h `elem` map index ns) $ M.assocs zs
                Just (Node _ l _ _)  = find ((== h) . index) rs
                in (closest 2 k,l)
        sq (k@(i,j),ns) = translate ((10 +) . fromIntegral $ i * 20) ((10 +) .fromIntegral $ j * 20) $ Pictures $ [color (greyN $ fromIntegral (length ns) / 10) $  rectangleSolid 20 20] 
        
        ci False (Node k (x,y) (vx,vy) (ax,ay)) = Translate x y $ (Color (greyN 0.5) (circle 5))
        ci True (Node k (x,y) (vx,vy) (ax,ay)) = Pictures [Translate x y $ (Color (greyN 0.5) (circle 5)), color red $ line [l,(x,y)]]

        in return $ Pictures $ (map sq $ M.assocs zs) ++ concatMap (\(z,xs) -> map (ci (z `elem` follow)) xs) (M.assocs zs) 
                
                

move :: Float -> (Int,Zones) -> IO (Int,Zones)
move t (h,zs) = do 
        zs' <- fmap zones . mapM (cinematic t) . concat . M.elems $ zs
        return (floor t  `mod` 240,zs')

cinematic _ (Node k (x,y) (vx,vy) (ax,ay)) = do
        dax <- randomRIO (-0.0001,0.0001)
        day <- randomRIO (-0.0001,0.0001)
        let 
                x' = x + vx
                y' = y + vy
                (x'',vx',ax') = if x' > 799 then 
                        (799,-1,0)
                        else if x' < 0 then
                                (0,1,0)
                                else (x',vx,ax)
                (y'', vy',ay') = if y' > 599 then 
                        (599, -1,0)
                        else if y' < 0 then
                                (0,1,0)
                                else (y',vy,ax)

        return $ Node
                k
                (x'',y'')
                (vx'+ ax ,vy' + ay)
                (dax + ax,day + ay)

scalar (x1,x2) (y1,y2) = x1 * y1 + x2 * y2
l *. (x2,y2) = (l * x2, l * y2)
(x1,y1) -. (x2,y2) = (x1 - x2, y1 - y2)
opposite (x,y) = (-x,-y)

hit (x1,v1) (x2,v2)  = let
        dx@(dxx,dxy) = x1 -. x2
        coeff = scalar (v1 -. v2) dx / (dxx ** 2 + dxy ** 2)
        dv = coeff *. dx
        v1' = v1 -. dv
        v2' = v2 -. opposite dv
        in (v1',v2')

