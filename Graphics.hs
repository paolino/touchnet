
{-# LANGUAGE ParallelListComp, ViewPatterns #-}
-- A nifty animated fractal of a tree, superimposed on a background 
--	of three red rectangles.

import Control.Concurrent.STM
import Control.Concurrent hiding (Chan)
import Prelude hiding (mapM, concat, foldr, concatMap, elem, sum)
import Graphics.Gloss
import System.Random
import Graphics.Gloss.Interface.IO.Game hiding (Key)
import Data.Foldable
import Data.Traversable
import qualified Data.Set as S
import System.Time
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
        mex <- newTVarIO $ Kolor 0 0 0 0
                
                
        playIO (InWindow "Zen" (800, 600) (5, 5))
                0
                45
                (mkWorld 0 30 10 20,Nothing,50,mex)
		(\w@(World t _,_,_,_) -> do 
                        atomically $ writeTVar runs t
                        render w
                        )
                (\e -> handle e)
                (\to (w@(World t _),p,fj,mex) -> seq to $ do
                        --atomically $ writeTVar runs t
                        return (stepWorld modNode w,p,fj,mex)
                        )




nodeState :: Close a Kolor -> Step a Kolor
nodeState (Close n f) = f n

data Kolor = Kolor 
        { kred::Float
        , kgreen :: Float
        , kblue :: Float
        , kalpha :: Float
        } deriving (Eq, Ord)

        
        


form :: Step Pos Kolor -> Picture
form (Transmit Common _ _) =  polygon $ regular 3
form (Transmit _ _ _) =  line $ regular 3
form (Receive _ Common _) = polygon $ regular 4
form (Receive _ _ _) = line $ regular 4
form (Sleep _) = line $ regular 5

zot (px,py) = ((px + 400)/800,(py + 300)/600)
unzot (px,py) = (px * 800 - 400, py * 600 - 300)

handle :: Event -> (World (Close Pos Kolor),Maybe Key,Int,TVar Kolor) -> IO (World (Close Pos Kolor),Maybe Key,Int,TVar Kolor)
handle (EventMotion (zot -> p)) (World i xs,Just k,jf,mex) = let
        Just (Close (Node ms _ ts rss ps l q w) f , rm) = select ((==k) . key . transmit . inspect) xs
        in return (World i $ rm (Close (Node ms p ts rss ps l q w) f) ,Just k,jf,mex)


handle (EventKey (MouseButton MiddleButton) Down (Modifiers Up Up Up) (zot -> p)) (World i xs,_,jf,mex) = do  
        r <- randomRIO (0,1) :: IO Float
        g <- randomRIO (0,1) :: IO Float
        b <- randomRIO (0,1) :: IO Float
        let     Close n  f : xs' = sortBy (comparing $ distance p . load . inspect) xs
                n' = n {store = insertMessage (Just (Kolor r g b 1)) $ store n}
        return (World i $ Close n' f : xs',Nothing,jf,mex)


handle (EventKey (SpecialKey KeySpace) Down (Modifiers Up Up Up) (zot -> p)) (w,k,jf,mex) =  do
        return (stepWorld modNode w,k,jf,mex)

handle (EventKey (MouseButton RightButton) Down (Modifiers Up Up Up) (zot -> p)) (World i xs,_,fj,mex) = return (World i $ x:xs,Nothing,fj + 10,mex) where
        x = closeU $ (\(Node _ a ts rss ps l q w) -> Node [Message 1 (Kolor 1 1 1 1)] a ts rss ps l q w) $ (mkNode fj 30 10 20){load = p}

handle (EventKey (MouseButton LeftButton) Down (Modifiers Up Up Up) (zot -> p)) (World i [],_,fj,mex) = return (World i [],Nothing,fj,mex)
handle (EventKey (MouseButton LeftButton) Down (Modifiers Up Up Up) (zot -> p)) (World i xs,_,fj,mex) = let
        Close (Node ms _ ts rss ps l q w) f : _ = sortBy (comparing $ distance p . load . inspect) xs
        in return (World i $ xs,Just (key ts),fj,mex)

handle (EventKey (MouseButton LeftButton) Up (Modifiers Up Up Up) (zot -> p)) (World i xs,_,fj,mex) = let
        in return (World i $ xs,Nothing,fj,mex)

handle _ x = return x

regular :: Int -> [(Float,Float)]
regular n = take (n + 1) $ map (\a -> (cos a,sin a)) [0,2*pi/fromIntegral n..]

fromKolor (Kolor r g b a) = makeColor r g b a

nodePicture (Node ms (x0,y0) _ _ _ _ _ _) = let
        l = length ms
        ns = regular l
        in translate x0 y0 . scale 20 20 $ Pictures [color (fromKolor n) . translate x y $ circle (pi/(fromIntegral l + 1)) | ((x,y),Message _ n) <- zip ns 
                (sortBy (comparing message) ms)]

render :: (World (Close Pos Kolor),Maybe Key, Int, TVar Kolor) -> IO Picture
render (World t xs,_,_,_) = let
        ns = map (inspect) xs 
        pos k = let   
                Just (load -> p) = find ((==) k . key . transmit) $ ns
                in p 
        in return . Pictures $ 
                [translate (-400) (000) $ scale 0.2 0.2 $ color (makeColor 0.3 0.3 0.3 0.3) $ Text "mouse left : move node"] ++ 
                [translate (-400) (-100) $ scale 0.2 0.2 $ color (makeColor 0.3 0.3 0.3 0.3) $ Text "mouse right : add node"] ++ 
                [translate (-400) (-200) $ scale 0.2 0.2 $ color (makeColor 0.3 0.3 0.3 0.3) $ Text "mouse middle : add fact"] ++ 
                (map (\((n,((x,y),(col,pub))),f) -> 
                        translate (x*800 - 400) (y*600 - 300) $ f ) . map ((id &&& load &&& collect &&& publicity) &&& nodePicture)   $ ns)
                ++ (concatMap (\(p,rs) -> 
                        map (\r -> color (cl $ snd r) $ line (map unzot $ triangolo p $ pos (key . fst $ r))) rs) . map (load &&& receives) $ ns)
                
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

triangolo p q = [p' `summa` scala 0.05 (dpq' `summa` per1), p' `summa` scala 0.8 dpq', p' `summa` scala 0.03 (dpq' `summa` per2),p' `summa` scala 0.03 (dpq' `summa` per1) ] where
        (per1,per2) = perp dpq'
        d = distance p q
        r = sqrt (800 ** 2 + 600 ** 2)
        dpq = diffa q p
        p' = p `summa` scala (11/r/d) dpq 
        q' = q `diffa` scala (11/r/d) dpq 
        dpq' = diffa q' p'




