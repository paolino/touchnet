
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
                -- print $ rs - a
                f rs
        forkIO $ f 0
                
                
        playIO (InWindow "Zen" (800, 600) (5, 5))
                0
                45
                (mkWorld 2 30 10 20,Nothing,50)
		(\w@(World t _,_,_) -> do 
                        atomically $ writeTVar runs t
                        render w
                        )
                (\e -> handle e)
                (\to (w@(World t _),p,fj) -> seq to $ do
                        --atomically $ writeTVar runs t
                        return (stepWorld modNode w,p,fj)
                        )




nodeState :: Close a Kolor -> Step a Kolor
nodeState (Close n f) = f n

data Kolor = Kolor 
        { kred::Float
        , kgreen :: Float
        , kblue :: Float
        , kalpha :: Float
        } deriving (Eq)

nodeKolor (Node ms _ _ _ _ _ _ _) = let 
        (Kolor r g b a,k) = foldr (\(Message n (Kolor r g b a)) (Kolor r1 g1 b1 a1, m) -> (
                Kolor (fromIntegral n  * r + r1) (fromIntegral n  * g + g1) (fromIntegral n  * b + b1) (fromIntegral n * a + a1),m + n)
                ) (Kolor 0 0 0 0,0) ms
        in case k of
                0 -> makeColor 1 1 1 1
                k -> makeColor (r / fromIntegral k) (g/fromIntegral k) (b/fromIntegral k) (a/fromIntegral k)

regular n = take (n + 1) $ map (\a -> (cos a,sin a)) [0,2*pi/fromIntegral n..]

form :: Step Pos Kolor -> Picture
form (Transmit Common _ _) =  polygon $ regular 3
form (Transmit _ _ _) =  line $ regular 3
form (Receive _ Common _) = polygon $ regular 4
form (Receive _ _ _) = line $ regular 4
form (Sleep _) = line $ regular 5

bestR f [] = Kolor 0 0 0 1
bestR f xs  = (\(Message i (Kolor r b g a)) -> Kolor (f r) b g a) . maximumBy (comparing (kred . message)) $ xs
bestG f [] = Kolor 0 0 0 1
bestG f xs = (\(Message i (Kolor r b g a)) -> Kolor r (f b) g a) . maximumBy (comparing (kgreen . message)) $ xs
bestB f [] = Kolor 0 0 0 1
bestB f xs = (\(Message i (Kolor r b g a)) -> Kolor r b (f g) a) . maximumBy (comparing (kblue . message)) $ xs
zot (px,py) = ((px + 400)/800,(py + 300)/600)
unzot (px,py) = (px * 800 - 400, py * 600 - 300)
handle (EventMotion (zot -> p)) (World i xs,Just k,jf) = let
        Just (Close (Node ms _ ts rss ps l q w) f , rm) = select ((==k) . key . transmit . inspect) xs
        in return (World i $ rm (Close (Node ms p ts rss ps l q w) f) ,Just k,jf)

handle (EventKey (MouseButton RightButton) Down (Modifiers Down Up Up) (zot -> p)) (World i xs,Nothing,jf) = let
        Close n  f : xs' = sortBy (comparing $ distance p . load . inspect) xs
        n' = n {store = insertMessage (Just $ bestB (\x -> (1 + x)/2) (store n)) $ store n}
        in return (World i $ Close n' f : xs',Nothing,jf)
handle (EventKey (MouseButton LeftButton) Down (Modifiers Down Up _) (zot -> p)) (World i xs,Nothing,jf) =  let
        Close n  f : xs' = sortBy (comparing $ distance p . load . inspect) xs
        n' = n {store = insertMessage (Just $ bestG (\x -> (1 + x)/2) (store n)) $ store n}
        in return (World i $ Close n' f : xs',Nothing,jf)

handle (EventKey (MouseButton MiddleButton) Down (Modifiers Down Up Up) (zot -> p)) (World i xs,Nothing,jf) = let
        Close n  f : xs' = sortBy (comparing $ distance p . load . inspect) xs
        n' = n {store = insertMessage (Just $ bestR (\x -> (1 + x)/2) (store n)) $ store n}
        in return (World i $ Close n' f : xs',Nothing,jf)


handle (EventKey (SpecialKey KeySpace) Down (Modifiers Up Up Up) (zot -> p)) (w,k,jf) = return (stepWorld modNode w,k,jf)

handle (EventKey (MouseButton RightButton) Down (Modifiers Up Up Up) (zot -> p)) (World i xs,_,fj) = return (World i $ x:xs,Nothing,fj + 10) where
        x = closeU $ (\(Node _ a ts rss ps l q w) -> Node [Message 1 (Kolor 0 1 0 1)] a ts rss ps l q w) $ (mkNode fj 30 10 20){load = p}

handle (EventKey (MouseButton LeftButton) Down (Modifiers Up Up Up) (zot -> p)) (World i xs,_,fj) = let
        Close (Node ms _ ts rss ps l q w) f : _ = sortBy (comparing $ distance p . load . inspect) xs
        in return (World i $ xs,Just (key ts),fj)
handle (EventKey (MouseButton LeftButton) Up (Modifiers Up Up Up) (zot -> p)) (World i xs,_,fj) = let
        in return (World i $ xs,Nothing,fj)
handle _ x = return x
render :: (World (Close Pos Kolor),Maybe Key, Int) -> IO Picture
render (World t xs,_,_) = let
        ns = map (inspect &&& nodeState) xs 
        pos k = let   
                Just (load -> p) = find ((==) k . key . transmit) $ map fst ns
                in p 
        in return $ 
                Pictures $ 
                        (map (\((n,((x,y),(col,pub))),f) -> color (nodeKolor n ) $ translate (x*800 - 400) (y*600 - 300) $ f ) . map ((id &&& load &&& collect &&& publicity) *** (scale 10 10 . form))   $ ns)
                        ++ (concatMap (\(p,rs) -> map (\r -> color (cl $ snd r) 
                                $ line (map unzot $ triangolo p $ pos (key . fst $ r))) rs)  $ map (load &&& receives) . map fst $ ns)
                
cl r    | r == 0 = makeColor 0 1 1 1
        | otherwise = makeColor 0 0 (1 - fromIntegral (negate r)/5)  1

cn False False = white
cn True False = yellow
cn False True = blue
cn True True = red
perp (x,y) = ((-y,x),(y,-x))
scala k (x,y) = (k*x,k*y)
summa (x1,y1) (x2,y2) = (x1 + x2,y1 + y2)
diffa (x1,y1) (x2,y2) = (x1 - x2,y1 - y2)

triangolo p q = [p' `summa` scala 0.05 (dpq' `summa` per1), p' `summa` scala 0.7 dpq', p' `summa` scala 0.03 (dpq' `summa` per2),p' `summa` scala 0.03 (dpq' `summa` per1) ] where
        (per1,per2) = perp dpq'
        d = distance p q
        r = sqrt (800 ** 2 + 600 ** 2)
        dpq = diffa q p
        p' = p `summa` scala (11/r/d) dpq 
        q' = q `diffa` scala (11/r/d) dpq 
        dpq' = diffa q' p'




