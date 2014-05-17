{-# LANGUAGE ViewPatterns, PatternSynonyms #-}

import System.Random (randomR, StdGen, randomRs, mkStdGen)
import Data.Maybe (catMaybes, isJust)
import Control.Arrow ((&&&), first, second)
import Data.List (lookup, groupBy, sortBy, mapAccumL)
import Data.Ord (comparing)
import Data.Function  (on)

-------------------------------------------------------------
-- | list rolling
roll :: [a] -> [a]
roll [] = []
roll (x:xs) = xs ++ [x]

-- | list modification
select :: (a -> Bool) -> [a] -> Maybe (a,a -> [a])
select f [] = Nothing
select f (x:xs) 
        | f x = Just (x,(:xs))
        | otherwise =  fmap (second $ fmap (x:)) $ select f xs
-------------------------------------------------------------


type Freq = Int

data Chan = Common | Chan Freq deriving (Eq,Ord)

data Seq a = Seq Int [a]

core (Seq i x) = x

dropSeq (Seq i (x:xs)) = Seq i xs

instance Eq (Seq a) where
        (==) (Seq i _) (Seq j _) = i == j

instance Show a => Show (Seq a) where
        show (Seq i xs) = show (i,take 5 xs)

type MF = Maybe Freq

data Node = Node {
        transmit :: Seq MF, -- ^ transmitting sequence
        receives :: [(Seq MF,Int)], -- ^ set of receiving sequences
        contract :: Seq Bool, -- ^ self spot sequence
        publicity :: Bool, -- ^ sync self spot condition
        collect :: Bool -- ^ sequence harvesting
        } deriving (Show)

-- | expose a node and a stepping , to permit node changing and query while hiding the stepping state
data Close = Close Node (Node -> Step)

-- | Stepping results. Any constructor result hold the new node. Receiving mode is completed with the received message
data Step      = Receive Chan (Maybe (Seq MF) -> Close)  -- ^ receive mode , possibly receive a seq on chan
                | Transmit Chan (Seq MF) Close  -- ^ transmit mode, send a seq on chan
                | Sleep Close  -- ^ sleep mode , just step

-- | split a list of Step 
partitionStep :: [Step] -> ([((Chan,Seq MF),Close)],[(Chan,Maybe (Seq MF) -> Close)],[Close])
partitionStep [] = ([],[],[])
partitionStep (Transmit c s n : xs) = let
        (ts,rs,ss) = partitionStep xs
        in (((c,s),n):ts,rs,ss)
partitionStep (Receive c f : xs) = let
        (ts,rs,ss) = partitionStep xs
        in (ts,(c,f):rs,ss)
partitionStep (Sleep n : xs) = let
        (ts,rs,ss) = partitionStep xs
        in (ts,rs,n:ss)

-- | view pattern that forget heads of receivers
stepReceivers :: [(Seq MF,Int)] -> [(Seq MF,Int)]
stepReceivers = map (first dropSeq)

-- | insert a new receiver in a node 
insertSeq :: Seq MF -> Node -> Node
insertSeq s n@(Node ts rss ps l q) 
                | s `elem` map fst rss  = n
                | otherwise = Node ts ((s,0) : rss) ps l q

-- | Stepping state
data Cond = MustTransmit | MustReceive | UnMust

-- | close a node and its future
close :: Cond -> Node -> Close
close c n = Close n (step c)

-- | specialized for unmust
closeU :: Node -> Close
closeU = close UnMust

-- | Core logic. Node fields are tested sequentially with pattern matching. First success fires the right Step
step :: Cond -> Node -> Step 
-- obliged to listen in the common chan after a publ
step MustReceive (Node (dropSeq -> ts) (stepReceivers -> rss) (dropSeq -> ps) l q) = Receive Common f where
        f Nothing = closeU (Node ts rss ps l q) 
        f (Just s) = closeU (insertSeq s $ Node ts rss ps l q) 
-- try to publ on a sync window
step  MustTransmit (Node (dropSeq -> ts) (stepReceivers -> rss) (dropSeq -> ps) l q) = Transmit Common ts . closeU $ Node ts rss ps l q
-- time to transmit: we transmit a receiving seq and roll the receiving seqs
step UnMust (Node (Seq i (Just c:ts)) (head &&& roll -> (x,rss)) (dropSeq -> ps) l q) 
        = Transmit (Chan c) (dropSeq . fst $ x) . closeU $ Node (Seq i ts) (stepReceivers rss) ps l q
--  time to listen on a receiving seq
step UnMust (Node (dropSeq -> ts) 
                (select (isJust . head . core . fst) -> Just ((Seq i (Just c:xs),n),g)) 
                (dropSeq -> ps) l q) = Receive (Chan c) f where
        new k = Node ts (stepReceivers $ g (Seq i $ Just c : xs,k)) ps l q
        f Nothing = closeU $ new $ n - 1
        f (Just s) = closeU $ insertSeq s . new $ 0
-- time to transmit on common chan our transmit seq, setting the duty to listen right after
step UnMust (Node (dropSeq -> ts) (stepReceivers -> rss) (Seq i (True:ps)) l q) = Transmit Common ts $ close MustReceive $ Node ts rss (Seq i ps) l q
-- time to receive freely on common channel
step UnMust (Node (dropSeq -> ts) (stepReceivers -> rss) (dropSeq -> ps) l (id &&& (l ||) -> (q,True))) = Receive Common f where
        f Nothing = closeU $ Node ts rss ps l q
        f (Just s) =  close (if l then MustTransmit else UnMust) $ (if q then insertSeq s else id) $ Node ts rss ps l q
-- time to sleep
step UnMust (Node (dropSeq -> ts) (stepReceivers -> rss) (dropSeq -> ps) l q) = Sleep . closeU $ Node ts rss ps l q

-- boot a node
mkStep :: Node -> Step
mkStep = Sleep . close UnMust


mkBoolSeq :: Int -> Int -> Seq Bool
mkBoolSeq n freq = Seq n $ map (==0) $ randomRs (0,freq) $ mkStdGen n

mkSeq :: Int -> Int -> Int -> Seq MF
mkSeq n chans freq = let
        Seq _ fs = mkBoolSeq n freq
        cs = randomRs (0,chans) $ mkStdGen $ n + 1
        in Seq n $ snd $ mapAccumL (\(c:cs) d -> if d then (cs, Just c) else (cs,Nothing)) cs fs

mkNode  :: Int -- ^ node unique id
        -> Int -- ^ number of channels
        -> Int -- ^ mean delta between data transmission
        -> Int -- ^ mean delta between self spots
        -> Node
mkNode ((*3) -> n) chans freq freqC = Node
        (mkSeq n chans freq)
        []
        (mkBoolSeq (n + 2) freqC)
        True
        True


-- | create the maxint distinct nodes
mkWorld chans freq freqC = [mkStep (mkNode i chans freq freqC) | i <- [0..]]

-- | eliminate unresponsive sequences
forget :: Int -> Node -> Node
forget n (Node ts rss ps l q) = Node ts (filter ((> negate n) . snd) rss)  ps l q

-- | check if node is indirectly spotted
listened :: Node -> Bool
listened (Node s rss _ _ _) = s `elem` map fst rss

inspect :: Step -> Node
inspect (Sleep (Close n _)) = n
inspect (Transmit _ _  (Close n _)) = n
inspect (Receive _ (($ Nothing) -> Close n _)) = n
{-

-- | A list of Nodes with a 2d pos
type World = [(Node, Pos)]

type Pos = (Float,Float)

(x1,x2) -. (y1,y2) = (x1-y1,x2-y2)
(x,y) ^-^ 2 = x ^ 2 + y ^ 2

distance r px py = (sqrt $ (px -. py) ^-^ 2) < r

stepWorld :: Int -> World -> World
stepWorld v ns =xs = map step ns 
        (unzip -> (ts,nts),rs,ss) = partitionStep xs
        ms = map head . filter ((==) 1 . length) . groupBy ((==) `on` fst ) . sortBy (comparing fst ) $ ts
        nrs = map f rs
        f (c,g) = g $ lookup c ms 
        in nts ++ map (forget v) nrs ++ ss
-}