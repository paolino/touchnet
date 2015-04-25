{-# LANGUAGE ViewPatterns, ImplicitParams, DeriveDataTypeable, TemplateHaskell, DeriveFunctor #-}
module Stepping where

import System.Random (randomR, StdGen, randomRs, mkStdGen)
import Data.Maybe (catMaybes, isJust,listToMaybe,isNothing)
import Control.Arrow ((&&&), (***), first, second)
import Data.List (lookup, groupBy, sortBy, mapAccumL)
import Data.Ord (comparing)
import Data.Function  (on)
import  qualified Data.Set as S
import Control.Lens (view,over, set,_2, Getting)
import Node
import Seq
import Timed
import List (roll, select)

-- | check the presence of a subvalue
elemBy :: Eq a => Getting a t a -> t -> [t] -> Bool 
elemBy l x = any $ (== view l x) . view l

-- insert a message if not present
insertMessage :: (?configuration :: Configuration, Eq m) => Maybe (Timed m) -> [Timed m] -> [Timed m]
insertMessage Nothing ms = ms
insertMessage (Just m) ms 
        | elemBy message m ms = ms -- what if the message is newer ? 
        | otherwise = take (memory ?configuration) $  m : ms 

-- | insert a new receiver in a node. The insertion in receivers happens if the sequence key is unknown.The chisentechi is updated always.
insertReceiver :: SeqT -> Key -> Node  m -> Node  m
insertReceiver s k n 
                | elemBy key s (view transmit n : map  (view transmissions) (view receives n)) = n'  -- checks it's us or a known 
                | otherwise                     = over receives (Neighbor s 1:) n' -- a new unreceived friend
        where n' = over chisentechi (S.insert (k, view key s)) n 

-- tailSeq for receivers
stepReceivers :: [Neighbor] -> [Neighbor]
stepReceivers = map (over transmissions tailSeq)

-- | Stepping state
data Cond       = MustTransmit -- ^ schedule a transmitting
                | MustReceive  -- ^ scheduled a receiving
                | UnMust  -- ^ free 

-- | close on a node and its future
close :: (?configuration :: Configuration, Eq m) => Cond -> Node  m -> Close  m
close c n = Close n (step c)

-- | Core logic. Node a fields are tested sequentially with pattern matching. First success fires the right Step
step    :: (?configuration :: Configuration, Eq m) 
        => Cond 
        -> Node  m 
        -> Step  m

-- Obliged to listen in the common chan. It happens after a publicity
step MustReceive (Node 
        (tailSeq -> hs) 
        (decTimeds -> ms) 
        (tailSeq -> ts) 
        (stepReceivers -> rss) 
        (tailSeq -> ps) l q chi
        ) = Receive  Common f where
                f Nothing = close UnMust (Node hs ms  ts rss ps l q chi)  
                f (Just (Comm s Nothing)) = close UnMust (insertReceiver s (view key s) $ Node hs ms  ts rss ps l q chi)  

-- try to publicize transmit seqs on a sync window on common channel, switch to must receive by contract (WRONG)
step MustTransmit (Node 
        (tailSeq -> hs) 
        (decTimeds -> ms) 
        (tailSeq -> ts) 
        (stepReceivers -> rss) 
        (tailSeq -> ps) l q chi
        ) = Transmit Common (Comm ts Nothing) . close MustReceive $ Node hs ms  ts rss ps l q chi

-- time to transmit a our transmit seq if no other seq is trustable 
step UnMust (Node 
        (tailSeq -> hs) 
        (decTimeds -> ms) 
        (Seq i (Just c:ts)) 
        (stepReceivers  &&& filter ((==0) . view misseds) -> (rss,[])) 
        (tailSeq -> ps) l q chi
        )  = Transmit (Chan c) (Comm (Seq i ts) $ listToMaybe ms) . close UnMust $ 
                Node hs (roll ms)  (Seq i ts) rss ps l q chi 

-- time to transmit a neighbor transmit seq and roll the neighbor seqs for fairness
step UnMust (Node 
        (tailSeq -> hs) 
        (decTimeds -> ms) 
        (Seq i (Just c:ts)) 
        ((roll  &&& filter ((==0). view misseds)). stepReceivers  -> (rss, x:_)) 
        (tailSeq -> ps) l q chi
        ) = Transmit (Chan c) (Comm (view transmissions x) $ listToMaybe ms) . close UnMust $ 
                Node hs (roll ms)  (Seq i ts) rss ps l q chi

--  time to listen on a receiving seq
step UnMust (Node 
                (tailSeq -> hs) 
                (decTimeds -> ms) 
                (tailSeq -> ts) 
                (select (isJust . head . view (transmissions . stream)) -> Just (Neighbor s@(Seq i (Just c : xs)) n, g))
                (tailSeq -> ps) l q chi
                ) = Receive  (Chan c) f where
                        new m n' = Node hs (insertMessage m ms)  ts (stepReceivers . g $ Neighbor s n') ps l q chi
                        f Nothing = close UnMust . new Nothing $ n + 1  -- missed appointment
                        f (Just (Comm s' m)) = close UnMust . insertReceiver s' i . new m $ 0 -- got it , set to trusted

-- insert a personal message with fresh ttl in the message list and sleep
step UnMust (Node 
        (Seq n (Just h:hs)) 
        (decTimeds -> ms) 
        (tailSeq -> ts) 
        (stepReceivers -> rss) 
        (tailSeq -> ps) l q chi
        ) = Sleep . close UnMust $ 
                Node (Seq n hs) (insertMessage (Just $ Timed (lmessagettl ?configuration) h) ms)  ts rss ps l q chi

-- time to transmit on common chan our transmit seq, setting the duty to listen right after, publicizing self transmittion times
step UnMust (Node 
        (tailSeq -> hs) 
        (decTimeds -> ms) 
        (tailSeq -> ts) 
        (stepReceivers -> rss) 
        (Seq i (True:ps)) l q chi
        ) = Transmit Common (Comm ts Nothing) $ close MustReceive $ Node hs ms  ts rss (Seq i ps) l q chi

-- time to receive freely on common channel, or we are free listening (high mode) or we are starving , noone is listening (publicizing our transmission seq)
step UnMust (Node 
        (tailSeq -> hs) 
        (decTimeds -> ms) 
        (tailSeq -> ts) 
        (stepReceivers -> rss) 
        (tailSeq -> ps) 
        l 
        (id &&& (l ||) -> (q,True)) chi
        ) = Receive  Common f where
                f Nothing = close UnMust $ Node hs ms  ts rss ps l q chi 
                f (Just (Comm s Nothing)) =  close (if l then MustTransmit else UnMust) $ 
                                (if q then insertReceiver s (view key s) else id) $ Node hs ms  ts rss ps l q chi 

-- time to sleep
step UnMust (Node hs (decTimeds -> ms) (tailSeq -> ts) (stepReceivers -> rss) (tailSeq -> ps) l q chi) = Sleep . close UnMust $ Node hs ms ts rss ps l q chi

-- boot a sleeping node
mkStep :: (?configuration :: Configuration,Eq m) => Node  m -> Step  m
mkStep = Sleep . close UnMust


mkBoolSeq       :: Key -- ^ random seed and identifier
                -> Int -- ^ frequency of True values
                -> Seq Bool
mkBoolSeq s@(Key n) freq = Seq s $ map (==0) $ randomRs (0,freq) $ mkStdGen n


-- | pseudo random sequence of Nothing and Just Freq
mkSeq   :: Key  -- ^ identifier, random seed
        -> Int  -- ^ channel space power
        -> Int  -- ^ mean delta between positives
        -> SeqM Int
mkSeq s@(Key n) chans freq = let
        Seq _ fs = mkBoolSeq s freq
        cs = randomRs (0,chans) $ mkStdGen $ n + 1
        in Seq s $ snd $ mapAccumL (\(c:cs) d -> (cs,if d then Just c else Nothing)) cs fs

mkSeqProd       :: Key
                -> Int
                -> m 
                -> SeqM m
mkSeqProd n freq x = Seq n [if y then Just x else Nothing | y <- view stream $ mkBoolSeq n freq] 

mkNode  :: (?configuration :: Configuration) 
        => Key -- ^ node unique id
        -> m  
        -> Node  m
mkNode ((*3) -> n) x = Node 
        (mkSeqProd (n + 2) (rodfrequency ?configuration) x)
        []  --messages
        (mkSeq n (numchannels ?configuration) (txfrequency ?configuration))
        [] -- receivers
        (mkBoolSeq (n + 1) (pufrequency ?configuration))
        True
        True
        S.empty


       


