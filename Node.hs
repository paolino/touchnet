{-# LANGUAGE ViewPatterns #-}
module Node where
import System.Random (randomR, StdGen, randomRs, mkStdGen)
import Data.Maybe (catMaybes, isJust)
import Control.Arrow ((&&&), (***), first, second)
import Data.List (lookup, groupBy, sortBy, mapAccumL)
import Data.Ord (comparing)
import Data.Function  (on)
import qualified Data.Set as S

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

-- | A radio channel
type Freq = Int

-- | Separation by construction of the link channel from the data channels
data Chan = Common | Chan Freq deriving (Eq,Ord)

-- | Sequence identifier
type Key = Int

-- | A sequence is an infinite list of something with an identifier 
data Seq a = Seq Key [a]

-- | extract key Seq
key :: Seq a -> Key
key (Seq k _) = k

-- | exctract the real sequence
core :: Seq a -> [a]
core (Seq i x) = x

-- | extract the tail of a Seq
tailSeq :: Seq a -> Seq a
tailSeq (Seq i (x:xs)) = Seq i xs

-- | match sequences by identifier . A lot of differentiating power is lost
instance Eq (Seq a) where
        (==) (Seq i _) (Seq j _) = i == j

-- | debugging instance
instance Show a => Show (Seq a) where
        show (Seq i xs) = show (i,take 5 xs)


-- | the value of sequences 
type MF = Maybe Freq

data Node a = Node {
        load :: a,
        transmit :: Seq MF, -- ^ transmitting sequence
        receives :: [(Seq MF,Int)], -- ^ set of receiving sequences
        contract :: Seq Bool, -- ^ self spot sequence
        publicity :: Bool, -- ^ sync self spot condition
        collect :: Bool, -- ^ sequence harvesting
        chisentechi :: S.Set (Key,Key) 
        } deriving (Show)

-- | expose a node and a stepping , to permit node changing and query while hiding the stepping state
data Close a = Close (Node a) (Node a -> Step a)

inspect (Close n f) = n
-- | Stepping results. Any constructor result hold the new node. Receiving mode is completed with the received message
data Step  a    = Receive a Chan (Maybe (Seq MF) -> Close a)  -- ^ receive mode , possibly receive a seq on chan
                | Transmit Chan (Seq MF) (Close a)  -- ^ transmit mode, send a seq on chan
                | Sleep (Close  a) -- ^ sleep mode , just step

-- | split a list of Step 
partitionStep :: [Step a] -> ([((Chan,Seq MF),Close a)],[(a,Chan,Maybe (Seq MF) -> Close a)],[Close a])
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

-- | view pattern that forget heads of receivers
stepReceivers :: [(Seq MF,Int)] -> [(Seq MF,Int)]
stepReceivers = map (first tailSeq)

-- | insert a new receiver in a node. The insertion in receivers happens if the sequence key is unknown.The chisentechi is updated always.
insertSeq :: Seq MF -> Key -> Node a -> Node a
insertSeq s@(Seq k1 _) k (Node a ts rss ps l q m) 
                | s `elem` (ts:map fst rss)     = Node a ts rss ps l q m'
                | otherwise                     = Node a ts ((s,-1) : rss) ps l q m'
        where m' = S.insert (k,k1) m

-- | Stepping state
data Cond = MustTransmit | MustReceive | UnMust

-- | close a node and its future
close :: Cond -> Node a -> Close a
close c n = Close n (step c)

-- | specialized for unmust
closeU :: Node a -> Close a
closeU = close UnMust

-- | Core logic. Node a fields are tested sequentially with pattern matching. First success fires the right Step
step :: Cond -> Node a -> Step  a
-- Obliged to listen in the common chan after a publ
step MustReceive (Node a (tailSeq -> ts) (stepReceivers -> rss) (tailSeq -> ps) l q m) = Receive a Common f where
        f Nothing = closeU (Node a ts rss ps l q m)  
        f (Just s) = closeU (insertSeq s (key s) $ Node a ts rss ps l q m)  
-- try to publ on a sync window on link channel
step MustTransmit (Node a (tailSeq -> ts) (stepReceivers -> rss) (tailSeq -> ps) l q m) = Transmit Common ts . close MustReceive $ Node a ts rss ps l q m
-- time to transmit: we transmit a receiving seq and roll the receiving seqs
step UnMust (Node a (Seq i (Just c:ts)) (id &&& filter ((==0) . snd) -> (rss,[])) (tailSeq -> ps) l q m)  
        = Transmit (Chan c) (Seq i ts) . closeU $ Node a (Seq i ts) (roll $ stepReceivers rss) ps l q m 
step UnMust (Node a (Seq i (Just c:ts)) (head . filter ((==0) . snd) &&& roll -> (x,rss)) (tailSeq -> ps) l q m) 
        = Transmit (Chan c) (tailSeq . fst $ x) . closeU $ Node a (Seq i ts) (stepReceivers rss) ps l q m
--  time to listen on a receiving seq
step UnMust (Node a (tailSeq -> ts) 
                (select (isJust . head . core . fst) -> Just ((Seq i (Just c : xs),n),g)) 
                (tailSeq -> ps) l q m) = Receive a (Chan c) f where
        new k = Node a ts (stepReceivers $ g (Seq i (Just c : xs),k)) ps l q m
        f Nothing = closeU $ new $ n - 1
        f (Just s) = closeU $ insertSeq s i. new $ 0
-- time to transmit on common chan our transmit seq, setting the duty to listen right after
step UnMust (Node a (tailSeq -> ts) (stepReceivers -> rss) (Seq i (True:ps)) l q m) = Transmit Common ts $ close MustReceive $ Node a ts rss (Seq i ps) l q m
-- time to receive freely on common channel
step UnMust (Node a (tailSeq -> ts) (stepReceivers -> rss) (tailSeq -> ps) l (id &&& (l ||) -> (q,True)) m) = Receive a Common f where
        f Nothing = closeU $ Node a ts rss ps l q m 
        f (Just s) =  close (if l then MustTransmit else UnMust) $ (if q then insertSeq s (key s) else id) $ Node a ts rss ps l q m 
-- time to sleep
step UnMust (Node a (tailSeq -> ts) (stepReceivers -> rss) (tailSeq -> ps) l q m) = Sleep . closeU $ Node a ts rss ps l q m

-- boot a node
mkStep :: Node a -> Step a
mkStep = Sleep . close UnMust


mkBoolSeq :: Int -> Int -> Seq Bool
mkBoolSeq n freq = Seq n $ map (==0) $ randomRs (0,freq) $ mkStdGen n


-- | pseudo random sequence of Nothing and Just Freq
mkSeq   :: Int  -- ^ identifier, random seed
        -> Int  -- ^ channel space power
        -> Int  -- ^ mean delta between positives
        -> Seq MF
mkSeq n chans freq = let
        Seq _ fs = mkBoolSeq n freq
        cs = randomRs (0,chans) $ mkStdGen $ n + 1
        in Seq n $ snd $ mapAccumL (\(c:cs) d -> if d then (cs, Just c) else (cs,Nothing)) cs fs

mkNode  :: Int -- ^ node unique id
        -> Int -- ^ number of channels
        -> Int -- ^ mean delta between data transmission
        -> Int -- ^ mean delta between self spots
        -> Node Pos
mkNode (n) chans freq freqC = Node 
        (fst . randomR (0,1) $ mkStdGen (n + 3), fst. randomR (0,1) $ mkStdGen (n + 4)) 
        (mkSeq n chans freq)
        []
        (mkBoolSeq (n + 2) freqC)
        True
        True
        S.empty



-- | eliminate unresponsive sequences
forget :: Int -> Node a -> Node a
forget n (Node a ts rss ps l q m) = Node a ts (filter ((> negate n) . snd) rss)  ps l q m


-- | partition listeners between node listener and unlistener 
partitionChiSenteChi :: Node a -> ([Key], [Key])
partitionChiSenteChi (Node a s _ _ _ _ m) = (map fst . S.toList *** map fst . S.toList) . S.partition ((== key s) . snd) $ m

test :: (a -> b) -> a -> (a,b)
test f = id &&& f

-- | empty the chisentechi bin
resetChiSenteChi :: Node a -> Node a
resetChiSenteChi n = n{chisentechi = S.empty}

every j i = i `mod` j == 0

modNode (every 10 -> True) n = resetChiSenteChi n
modNode _ n@(Node a ts rss ps _ _ w) = forget 5 $ Node a ts rss ps ((<2) . length . fst . partitionChiSenteChi $ n) ((<2) . length $ rss) w


data World a = World Int [a] deriving (Show)

elems (World i xs) = xs

instance Functor World where
        f `fmap` World i x = World i (f `fmap` x)
       
type Pos = (Float,Float)
distance (x,y) (x1,y1) = sqrt $ (x - x1) ** 2 + (y - y1) ** 2

stepWorld :: (Int -> Node Pos -> Node Pos) -> World (Close Pos) -> World (Close Pos)
stepWorld v (World n cs) = let 
        xs = map (\(Close no f) -> f(v n no)) cs :: [Step Pos] 
        (ts,rs,ss) = partitionStep xs 
        ms t = map (fst . head) . groupBy ((==) `on` (fst . fst)) . sortBy (comparing $ fst . fst) $
                 filter ((< 0.3) . distance t . load . inspect . snd) $ ts :: [(Chan,Seq MF)]
        f (t,c,g) = g $ lookup c (ms t)
        nrs = map f rs
        in World (n + 1) $ map snd ts ++ nrs ++ ss

-- | create the maxint distinct nodes
mkWorld :: Int -> Int -> Int -> Int -> World (Close Pos)
mkWorld z chans freq freqC = World 0 $ take z [Close (mkNode  i chans freq freqC) (step UnMust) | i <- [0,10..]] where
       

{-
-- | A list of Node as with a 2d pos

type Pos = (Float,Float)

(x1,x2) -. (y1,y2) = (x1-y1,x2-y2)
(x,y) ^-^ 2 = x ^ 2 + y ^ 2

distance r px py = (sqrt $ (px -. py) ^-^ 2) < r

-}
