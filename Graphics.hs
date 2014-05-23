
{-# LANGUAGE ParallelListComp, ViewPatterns, DeriveDataTypeable, ImplicitParams #-}
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
import Data.List (sortBy, sort)
import Data.Ord (comparing)
import Control.Arrow
import Node
import Control.Monad
import Data.Typeable
import System.Console.CmdArgs

type Pos = (Float,Float)
distance (x,y) (x1,y1) = sqrt $ (x - x1) ** 2 + (y - y1) ** 2

sample = Configuration 
        {       frames = def &= help "number of frames per second" &= opt (25 :: Int)
        ,       txfrequency = def &= help "mean delta in frames between node transmission events" &= opt (10 :: Int)
        ,       pufrequency = def &= help "mean delta in frames between node self publicity events" &= opt (20 :: Int)
        ,       rodfrequency = def &= help "mean delta in frames between node fact roduction event" &= opt (20 :: Int)
        ,       memory = def &= help "node fact memory capcacity" &= opt (5 :: Int)
        ,       numchannels = def &= help "channel diversity" &= opt (30 :: Int)
     
        ,       appargs = def &= help "radio range in video units"  &= opt (0.3::Float)
        }
         &= summary "Simulation of touchnet protocol"


main :: IO ()
main = 	do
        args <- cmdArgs sample
        let ?configuration = args in playIO (InWindow "Zen" (800, 600) (5, 5))
                0
                (frames ?configuration)
                (World 0 [],Nothing,0)
		render
                handle
                (\to (w@(World t _),p,fj) -> seq to $ do
                        return (stepWorld (\x -> (< appargs args) . distance x)  modNode w,p,fj)
                        )

nodeState :: Close a Kolor -> Step a Kolor
nodeState (Close n f) = f n

data Kolor = Kolor 
        { kred::Float
        , kgreen :: Float
        , kblue :: Float
        , kalpha :: Float
        } deriving (Eq, Ord)
data Letter = Letter Char deriving (Eq, Ord)

zot (px,py) = ((px + 400)/800,(py + 300)/600)
unzot (px,py) = (px * 800 - 400, py * 600 - 300)

handle :: (?configuration :: Configuration Float) => Event -> (World (Close Pos Letter),Maybe Key,Int) -> IO (World (Close Pos Letter),Maybe Key,Int)
handle (EventMotion (zot -> p)) (World i xs,Just k,jf) = let
        Just (Close (Node hs ms _ ts rss ps l q w) f , rm) = select ((==k) . key . transmit . inspect) xs
        in return (World i $ rm (Close (Node hs ms p ts rss ps l q w) f) ,Just k,jf)


handle (EventKey (Char c) Down (Modifiers Up Up Up) (zot -> p)) (World i xs,_,jf) = do  
        let     Close n  f : xs' = sortBy (comparing $ distance p . load . inspect) xs
                n' = n {roduction = fmap (fmap (const $ Letter c)) $ roduction n}
        return (World i $ Close n' f : xs',Nothing,jf)



handle (EventKey (MouseButton RightButton) Down (Modifiers Up Up Up) (zot -> p)) (World i xs,_,fj) = return (World i $ x:xs,Nothing,fj + 1) where
        x = closeU $ (\(Node xs _ a ts rss ps l q w) -> Node xs [] a ts rss ps l q w) $ mkNode fj p $ Letter (toEnum $ fromEnum 'a' + fj)

handle (EventKey (MouseButton LeftButton) Down (Modifiers Up Up Up) (zot -> p)) (World i [],_,fj) = return (World i [],Nothing,fj)

handle (EventKey (MouseButton LeftButton) Down (Modifiers Up Up Up) (zot -> p)) (World i xs,_,fj) = let
        Close (Node hs ms _ ts rss ps l q w) f : _ = sortBy (comparing $ distance p . load . inspect) xs
        in return (World i $ xs,Just (key ts),fj)

handle (EventKey (MouseButton LeftButton) Up (Modifiers Up Up Up) (zot -> p)) (World i xs,_,fj) = let
        in return (World i $ xs,Nothing,fj)

handle _ x = return x

regular :: Int -> [(Float,Float)]
regular n = take (n + 1) $ map (\a -> (cos a,sin a)) [0,2*pi/fromIntegral n..]


nodePicture (Node _ ms (x0,y0) _ _ _ _ _ _) = translate x0 y0 . scale 20 20 $ Pictures  $

                [color white $ scale 0.01 0.01 $ text $ map (\(Letter c) -> c)  $ sort ms]

render :: (World (Close Pos Letter),Maybe Key, Int) -> IO Picture
render (World t xs,_,_) = let
        ns = map (inspect) xs 
        pos k = let   
                Just (load -> p) = find ((==) k . key . transmit) $ ns
                in p 
        in return . Pictures $ 
                [translate (-400) (-200) $ scale 0.2 0.2 $ color (makeColor 0.3 0.3 0.3 0.3) $ Text "mouse left : move node"] ++ 
                [translate (-400) (-250) $ scale 0.2 0.2 $ color (makeColor 0.3 0.3 0.3 0.3) $ Text "mouse right : add node"] ++ 
                (map (\((n,((x,y),(col,pub))),f) -> 
                        translate (x*800 - 400) (y*600 - 300) $ f ) . map ((id &&& load &&& collect &&& publicity) &&& nodePicture)   $ ns)
                ++ (concatMap (\(p,rs) -> 
                        map (\r -> color (cl $ snd r) $ line (map unzot $ triangolo p $ pos (key . fst $ r))) rs) . map (load &&& receives) $ ns)
                
cl r    | r == 0 = makeColor 1 0 0 1
        | otherwise = makeColor 0 0 ((1 - fromIntegral (negate r)/5)/2)  1


triangolo p q = [p' `summa` scala 0.05 (dpq' `summa` per1)
                , p' `summa` scala 0.8 dpq'
                , p' `summa` scala 0.03 (dpq' `summa` per2)
                ,p' `summa` scala 0.03 (dpq' `summa` per1) ] where
        (per1,per2) = perp dpq'
        d = distance p q
        r = sqrt (800 ** 2 + 600 ** 2)
        dpq = diffa q p
        p' = p `summa` scala (11/r/d) dpq 
        q' = q `diffa` scala (11/r/d) dpq 
        dpq' = diffa q' p'
        perp (x,y) = ((-y,x),(y,-x))
        scala k (x,y) = (k*x,k*y)
        summa (x1,y1) (x2,y2) = (x1 + x2,y1 + y2)
        diffa (x1,y1) (x2,y2) = (x1 - x2,y1 - y2)



