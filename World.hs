{-# LANGUAGE ViewPatterns, ImplicitParams, DeriveDataTypeable, TemplateHaskell, DeriveFunctor,DataKinds, TemplateHaskell, GADTs #-}
-- | A world is a set of positioned futures.
-- Each step each Future is fed with its node wich 
module World where

-- import System.Random (randomR, StdGen, randomRs, mkStdGen)
import Data.Maybe (catMaybes, isJust,listToMaybe)
import Control.Arrow ((&&&), (***), first, second)
import Data.List (lookup, groupBy, sortBy, mapAccumL, deleteFirstsBy)
import Data.Ord (comparing)
import Data.Function  (on)
import Control.Lens (view,over, set,_2, toListOf)
import Control.Lens.TH
import Node
import Seq
import Timed
import List (roll, select)
import Stepping
import Positioned

data WorldConfiguration = WorldConfiguration {
        radiorange :: Double
        }

data World a m = World {
        _nodes :: [Positioned (a m)] -- 
        , _topkey :: Key -- ^ random key counter
        }
makeLenses ''World

-- helper to match on positioned internals
stepPat :: Positioned a -> (Positioned a, a)
stepPat = id &&& view value

-- | compute the futures for state values. Sleep an transitting are just picked out, receivers are fed with a message when present
stepState :: (?configuration :: WorldConfiguration) => [Positioned (State m)] -> Positioned (State m) -> Positioned (Future m)

stepState _ (stepPat -> (p,Sleep x))                    = set value x p
stepState _ (stepPat -> (p,TransmitCommon _ _ x))       = set value x p
stepState _ (stepPat -> (p,TransmitFree _ _ x))         = set value x p

stepState xs (stepPat -> (p,ReceiveCommon Common f))    = case isolated (radiorange ?configuration) p (transmittedCommon xs) of
        Nothing -> set value (f Nothing) p
        Just (view value ->  TransmitCommon _ m _) -> set value (f $ Just m) p

stepState xs (stepPat -> (p,ReceiveFree c f))           = case isolated (radiorange ?configuration) p (transmittedFree c xs) of
        Nothing -> set value (f Nothing) p
        Just (view value ->  TransmitFree _ m _) -> set value (f $ Just m) p

-- stepping a world of futures by applying the nodes to their continuation and stepping each state
stepWorld :: (?configuration :: WorldConfiguration) => World Future m -> World Future m
stepWorld = over nodes $ (flip map <*> stepState) . map (fmap applyFuture)

-- transmitters on common channel
transmittedCommon ::  [Positioned (State t)] -> [Positioned (State t)]
transmittedCommon = filter (select . view value)   where
        select (TransmitCommon _ _ _) = True
        select _ = False

-- transmitters on free channel 
transmittedFree :: Chan TFree -> [Positioned (State t)] -> [Positioned (State t)]
transmittedFree c = filter (select . view value)   where
        select (TransmitFree c' _ _) = c == c'
        select _ = False

-- | remove the nearest to given coordinates node if possible
remove :: Double -> Double -> World Future m -> World Future m
remove x y = over nodes $ remove' x y where
        remove' _ _ [] = []
        remove' x y xs = tail . sortBy (comparing (distance (Positioned () x y))) $ xs

-- | add a new node at the given coordinates
add :: (?wconf:: WorldConfiguration, ?nconf:: NodeConfiguration, Eq m) => Double -> Double  -> m -> World Future m -> World Future m
add x y m (World zs k) = World (Positioned z x y : zs) k' where
        (k',z) = mkFuture k m

-- | visit nodes
worldNodes :: World Future m -> [Positioned (Node m)] 
worldNodes = over (traverse . value) (view node) . view nodes
        
