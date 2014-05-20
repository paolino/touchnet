
{-# LANGUAGE ParallelListComp, ViewPatterns #-}
-- A nifty animated fractal of a tree, superimposed on a background 
--	of three red rectangles.

import Control.Concurrent.STM
import Control.Concurrent hiding (Chan)
import Prelude hiding (mapM, concat, foldr, concatMap, elem, sum)
import Graphics.Gloss
import System.Random
import Graphics.Gloss.Interface.IO.Game hiding (Key)
import qualified Data.IntMap.Strict as I
import qualified Data.Map.Strict as M
import Data.Foldable
import Data.Traversable
import qualified Data.Set as S
import System.Time
import Control.DeepSeq
import Data.List (sortBy)
import Data.Ord (comparing)
import Control.Arrow
import Node
import Control.Monad

millis :: IO Integer
millis = getClockTime >>= \(TOD x y) -> return $ x * 1000 + (y `div` 1000000000)

main :: IO ()
main 
-- main = mapM_ print . elems . fmap inspect $ (!!1000) . iterate (stepWorld modNode) $ mkWorld 100 10 3 8
 = 	do
        runs <- newTVarIO 0
        let f a = do
                threadDelay 1000000
                rs <- atomically $ readTVar runs                
                print $ rs - a
                f rs
        forkIO $ f 0
                
                
        playIO (InWindow "Zen" (800, 600) (5, 5))
                0
                25
                (mkWorld 10 30 10 25,Nothing)
		(render)
                (\e -> handle e)
                (\_ (w@(World t _),p) -> do
                        atomically $ writeTVar runs t
                        return (stepWorld modNode w,p)
                        )


nodeState :: Close a -> Step a
nodeState (Close n f) = f n

regular n = take (n + 1) $ map (\a -> (cos a,sin a)) [0,2*pi/fromIntegral n..]
form :: Step Pos -> Picture
form (Transmit Common _ _) =  polygon $ regular 3
form (Transmit _ _ _) =  line $ regular 3
form (Receive _ Common _) = polygon $ regular 4
form (Receive _ _ _) = line $ regular 4
form (Sleep _) = line $ regular 5


zot (px,py) = ((px + 400)/800,(py + 300)/600)
unzot (px,py) = (px * 800 - 400, py *600 - 300)
handle (EventMotion (zot -> p)) (World i xs,Just k) = let
        Just (Close (Node _ ts rss ps l q w) f , rm) = select ((==k) . key . transmit . inspect) xs
        in return (World i $ rm (Close (Node p ts rss ps l q w) f) ,Just k)

handle (EventKey (SpecialKey KeySpace) Down (Modifiers Up Up Up) (zot -> p)) (w,k) = return (stepWorld modNode w,k)

handle (EventKey (MouseButton LeftButton) Down (Modifiers Up Up Up) (zot -> p)) (World i xs,_) = let
        Close (Node _ ts rss ps l q w) f : _ = sortBy (comparing $ distance p . load . inspect) xs
        in return (World i $ xs,Just (key ts))
handle (EventKey (MouseButton LeftButton) Up (Modifiers Up Up Up) (zot -> p)) (World i xs,_) = let
        in return (World i $ xs,Nothing)
handle _ x = return x
render :: (World (Close Pos),Maybe Key) -> IO Picture
render (World t xs,_) = let
        ns = map (inspect &&& nodeState) xs 
        pos k = let   
                Just (load -> p) = find ((==) k . key . transmit) $ map fst ns
                in p 
        in return $ 
                Pictures $ 
                        (map (\(((x,y),(col,pub)),f) -> color (cn col pub) $ translate (x*800 - 400) (y*600 - 300) $ f ) . map ((load &&& collect &&& publicity) *** (scale 10 10 . form))   $ ns)
                        ++ (concatMap (\(p,rs) -> map (\r -> color (cl $ snd r) 
                                $ line (map unzot $ triangolo p $ pos (key . fst $ r))) rs)  $ map (load &&& receives) . map fst $ ns)
                
cl r    | r == 0 = makeColor 0 1 1 1
        | otherwise = makeColor 0 0 (1 - fromIntegral (negate r)/3)  1
cn False False = white
cn True False = yellow
cn False True = blue
cn True True = red
perp (x,y) = ((-y,x),(y,-x))
scala k (x,y) = (k*x,k*y)
summa (x1,y1) (x2,y2) = (x1 + x2,y1 + y2)
diffa (x1,y1) (x2,y2) = (x1 - x2,y1 - y2)

triangolo p q = [p' `summa` scala 0.05 (dpq' `summa` per1), q', p' `summa` scala 0.03 (dpq' `summa` per2),p' `summa` scala 0.03 (dpq' `summa` per1) ] where
        (per1,per2) = perp dpq'
        d = distance p q
        r = sqrt (800 ** 2 + 600 ** 2)
        dpq = diffa q p
        p' = p `summa` scala (11/r/d) dpq 
        q' = q `diffa` scala (11/r/d) dpq 
        dpq' = diffa q' p'




