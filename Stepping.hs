{-# LANGUAGE ViewPatterns, ImplicitParams, DeriveDataTypeable, TemplateHaskell, DeriveFunctor, GADTs #-}
module Stepping where

import System.Random (randomR, StdGen, randomRs, mkStdGen)
import Data.Maybe (catMaybes, isJust,listToMaybe,isNothing, fromJust)
import Control.Arrow ((&&&), (***), first, second)
import Data.List (lookup, groupBy, sortBy, mapAccumL)
import Data.Ord (comparing)
import Data.Function  (on)
import  qualified Data.Set as S
import Control.Lens (view,over, set,_2, Getting, to)
import Node
import Seq
import Timed
import List (roll, select)



-- | Stepping state
data Cond     where 
        MustTransmit :: Cond  -- ^ schedule a transmitting
        MustReceive :: Cond  -- ^ scheduled a receiving
        UnMust :: Cond  -- ^ free 

-- | close on a node and its future
close :: (?nconf :: NodeConfiguration, Eq m) => Cond -> Node  m -> Future  m
close c n = Future n (step c)

-- | Core logic. Node a fields are tested sequentially with pattern matching. 
-- First success fires the right Step
step    :: (?nconf :: NodeConfiguration, Eq m) 
        => Cond 
        -> Node  m 
        -> State  m
 
-- Obliged to listen in the common chan. It happens after a publicity. 
-- This has to be respected by contract with other nodes
step MustReceive (Node 
        (tailSeq -> hs) 
        (decTimeds -> ms) 
        (tailSeq -> ts) 
        (tailNeighbors -> rss) 
        (tailSeq -> ps) 
        ls
        ) = ReceiveCommon  Common f where
                f Nothing = close UnMust $ Node hs ms ts rss ps ls
                f (Just (Publ s)) = close UnMust (addNeighbor s $ Node hs ms  ts rss ps ls) -- doesn't use sync to avoid loop 

-- try to publicize transmit seqs on a sync window on common channel, switch to must receive by contract.
step MustTransmit (Node 
        (tailSeq -> hs) 
        (decTimeds -> ms) 
        (tailSeq -> ts) 
        (tailNeighbors -> rss) 
        (tailSeq -> ps) 
        ls
        ) = TransmitCommon Common (Publ ts) . close MustReceive $ Node hs ms  ts rss ps ls

-- time to transmit our transmit seq if no other seq is trustable 
step UnMust (Node 
        (tailSeq -> hs) 
        (decTimeds -> ms) 
        (Seq i (Just c:ts)) 
        (tailNeighbors  &&& filter ((==0) . view misseds) -> (rss,[])) 
        (tailSeq -> ps) 
        ls
        )  = TransmitFree (Free c) (Info (Seq i ts) $ listToMaybe ms) . close UnMust $ 
                Node hs (roll ms)  (Seq i ts) rss ps ls

-- time to transmit a neighbor transmit seq and roll the neighbor seqs for fairness
step UnMust (Node 
        (tailSeq -> hs) 
        (decTimeds -> ms) 
        (Seq i (Just c:ts)) 
        ((roll  &&& filter ((==0). view misseds)). tailNeighbors  -> (rss, x:_)) 
        (tailSeq -> ps) 
        ls
        ) = TransmitFree (Free c) (Info (view transmissions x) $ listToMaybe ms) . close UnMust $ 
                Node hs (roll ms)  (Seq i ts) rss ps ls

--  time to listen on a receiving seq
step UnMust (Node 
                (tailSeq -> hs) 
                (decTimeds -> ms) 
                (tailSeq -> ts) 
                (select (isJust . head . view (transmissions . stream)) -> Just (Neighbor s n, g))
                (tailSeq -> ps) 
                ls
                ) = ReceiveFree  (Free $ fromJust . head . view stream $ s) $ close UnMust . f where
                        add n' = clean $ Node hs ms ts (tailNeighbors . g $ Neighbor s n') ps ls
                        f Nothing = add $ n + 1  -- missed appointment
                        f (Just (Info s' (Just m))) = addMessage m . addNeighborOrListener s s'  . add $ 0 
                                -- got it , set to trusted
                        f (Just (Info s' Nothing)) = addNeighborOrListener s s'. add $ 0 -- got it , set to trusted

-- insert a personal message with fresh ttl in the message list and sleep
step UnMust (Node 
        (Seq n (Just h:hs)) 
        (decTimeds -> ms) 
        (tailSeq -> ts) 
        (tailNeighbors -> rss) 
        (tailSeq -> ps)
        ls
        ) = Sleep . close UnMust $ addMessage  (Timed (lmessagettl ?nconf) h) $ 
                Node (Seq n hs) ms ts rss ps ls

-- time to transmit on common chan our transmit seq, setting the duty to listen right after, view (transmit . key) n
-- publicizing self transmittion times
step UnMust (Node 
        (tailSeq -> hs) 
        (decTimeds -> ms) 
        (tailSeq -> ts) 
        (tailNeighbors -> rss) 
        (Seq i (True:ps)) 
        ls
        ) = TransmitCommon Common (Publ ts) $ close MustReceive $
                 Node hs ms  ts rss (Seq i ps) ls
-- time to receive freely on common channel and use sync to force our presence on the neighbor 
step UnMust (shouldSync &&& id -> (True, Node 
        (tailSeq -> hs) 
        (decTimeds -> ms) 
        (tailSeq -> ts) 
        (tailNeighbors -> rss) 
        (tailSeq -> ps) 
        ls
        )) = ReceiveCommon Common f where
                f Nothing = close UnMust $ Node hs ms  ts rss ps ls
                f (Just (Publ s)) 
                        | not (view key s `elem`  ls) = close MustTransmit $ addNeighbor s $ 
                                Node hs ms  ts rss ps ls -- only reply on non publicizing neighbor
                        | otherwise = close UnMust $ addNeighbor s  $ 
                                Node hs ms  ts rss ps ls

-- time to receive freely on common channel, missing neighbors
step UnMust (shouldListen &&& id -> (True, Node 
        (tailSeq -> hs) 
        (decTimeds -> ms) 
        (tailSeq -> ts) 
        (tailNeighbors -> rss) 
        (tailSeq -> ps) 
        ls
        )) = ReceiveCommon  Common f where
                f Nothing = close UnMust $ Node hs ms  ts rss ps ls
                f (Just (Publ s)) =  close UnMust $ addNeighbor s $ Node hs ms  ts rss ps ls

-- time to sleep
step UnMust (Node hs (decTimeds -> ms) (tailSeq -> ts) (tailNeighbors -> rss) (tailSeq -> ps) ls) = 
        Sleep . close UnMust $ Node hs ms ts rss ps ls



mkFuture  :: (Eq m, ?nconf :: NodeConfiguration) 
        => Key -- ^ node unique id
        -> m  -- ^ fixed node message
        -> (Key, Future m)
mkFuture n m = second (flip Future $ step UnMust) (mkNode n m) 


