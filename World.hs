
{-# LANGUAGE ViewPatterns, ImplicitParams, DeriveDataTypeable, TemplateHaskell, DeriveFunctor #-}
module World where

-- import System.Random (randomR, StdGen, randomRs, mkStdGen)
import Data.Maybe (catMaybes, isJust,listToMaybe)
import Control.Arrow ((&&&), (***), first, second)
import Data.List (lookup, groupBy, sortBy, mapAccumL)
import Data.Ord (comparing)
import Data.Function  (on)
import  qualified Data.Set as S
import Control.Lens (view,over, set,_2)
import Node
import Seq
import Timed
import List (roll, select)

import Node
import Stepping

-- | eliminate unresponsive sequences
forget :: Int -> Node a m -> Node a m
forget i = over receives (filter ((> i) . snd))

-- | switch full listening state
switchListener :: Node a m -> Node a m
switchListener n = set publicity  ((<2) . length . fst . partitionChiSenteChi $ n) n


-- | partition listeners between node listener and unlistener 
partitionChiSenteChi :: Node a m -> ([Key], [Key])
partitionChiSenteChi n = (map fst . S.toList *** map fst . S.toList) . S.partition ((== view (transmit . key) n) . snd) $ view chisentechi n


-- | empty the chisentechi bin
resetChiSenteChi :: Node a m -> Node a m
resetChiSenteChi  = set chisentechi S.empty

every j i = i `mod` j == 0


modNode (every 30 -> True) n = resetChiSenteChi n
modNode _ n@(Node hs ms a ts rss ps _ _ w) = forget 5 . switchListener $ Node hs ms a ts rss ps  ((<2) . length $ rss) w

data World a m = World 
        Int -- ^ frame count
        [Close a m] -- ^ set of nodes

-- | split a list of Step 
partitionStep :: [Step a m] -> (
        [((Chan,(SeqM Freq,  Maybe (Timed m))),Close a m)],
        [(a,Chan,Maybe (SeqM Freq,  Maybe (Timed m)) -> Close a m)],
        [Close a m]
        )
partitionStep [] = ([],[],[])
partitionStep (Transmit c s n : xs) = let
        (ts,rs,ss) = partitionStep xs
        in (((c,s),n):ts,rs,ss)
partitionStep (Receive x c f : xs) = let
        (ts,rs,ss) = partitionStep xs
        in (ts,(x,c,f):rs,ss)
partitionStep (Sleep n : xs) = let
        (ts,rs,ss) = partitionStep xs
        in (ts,rs,n:ss)

stepWorld       :: (a -> a -> Bool) 
                -> World a m 
                -> World a m 
stepWorld ckd (World n cs) = let 
        xs = map (\(Close no f) -> f (modNode n no)) cs -- :: [Step Pos m] 
        (ts,rs,ss) = partitionStep xs 
        ms t = map (fst . head) . groupBy ((==) `on` (fst . fst)) . sortBy (comparing $ fst . fst) $
                 filter (ckd t . view (_2 . node . load )) $ ts -- :: [(Chan,Seq MF)]
        f (t,c,g) = g $ lookup c (ms t)
        rs' = map f rs
        in World (n + 1) $ map snd ts ++ rs' ++ ss

