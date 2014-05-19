
{-# LANGUAGE ParallelListComp, ViewPatterns #-}
-- A nifty animated fractal of a tree, superimposed on a background 
--	of three red rectangles.

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

import Node

main :: IO ()
main 
-- main = mapM_ print . elems . fmap inspect $ (!!1000) . iterate (stepWorld modNode) $ mkWorld 100 10 3 8
 = 	playIO (InWindow "Zen" (800, 600) (5, 5))
                0
                50
                (mkWorld 8 10 3 8,Nothing)
		(render)
                (\e -> handle e)
                (\_ (w,p) -> return (stepWorld modNode w,p))

zot (px,py) = ((px + 400)/800,(py + 300)/600)
unzot (px,py) = (px * 800 - 400, py *600 - 300)
handle (EventMotion (zot -> p)) (World i xs,Just k) = let
        Just (Close (Node _ ts rss ps l q w) f , rm) = select ((==k) . key . transmit . inspect) xs
        in return (World i $ rm (Close (Node p ts rss ps l q w) f) ,Just k)

handle (EventKey (MouseButton LeftButton) Down (Modifiers Up Up Up) (zot -> p)) (World i xs,_) = let
        Close (Node _ ts rss ps l q w) f : _ = sortBy (comparing $ distance p . load . inspect) xs
        in return (World i $ xs,Just (key ts))
handle (EventKey (MouseButton LeftButton) Up (Modifiers Up Up Up) (zot -> p)) (World i xs,_) = let
        in return (World i $ xs,Nothing)
handle _ x = return x
render :: (World (Close Pos),Maybe Key) -> IO Picture
render (World t xs,_) = let
        ns = map inspect xs 
        in return $ Pictures $ map (\(x,y) -> color white $ translate (x*800 - 400) (y*600 - 300) $ circle 10 ) . map load $ ns
                
                




