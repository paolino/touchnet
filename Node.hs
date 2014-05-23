{-# LANGUAGE ViewPatterns, ImplicitParams, DeriveDataTypeable #-}
module Node where
import System.Random (randomR, StdGen, randomRs, mkStdGen)
import Data.Maybe (catMaybes, isJust,listToMaybe)
import Control.Arrow ((&&&), (***), first, second)
import Data.List (lookup, groupBy, sortBy, mapAccumL)
import Data.Ord (comparing)
import Data.Function  (on)
import Data.Typeable
import Data.Data
import qualified Data.Set as S

data Configuration c = Configuration {
        frames :: Int,
        txfrequency :: Int,
        pufrequency :: Int,
        rodfrequency :: Int, 
        memory :: Int,
        numchannels :: Int,
        appargs :: c
        } deriving (Show, Typeable, Data)

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
pick :: (a -> Bool) -> [a] -> Maybe (a,[a])
pick f [] = Nothing
pick f (x:xs)
        | f x = Just (x,xs)
        | otherwise = fmap (second (x:)) $ pick f xs

toHead f g  = fmap (\(x,xs) -> g x : xs) . pick f 

-------------------------------------------------------------

-- | A radio channel
type Freq = Int

-- | Separation by construction of the link channel from the data channels
data Chan = Common | Chan Freq deriving (Eq,Ord)

-- | Sequence identifier
type Key = Int

-- | A sequence is an infinite list of something with an identifier 
data Seq a = Seq Key [a]
instance Functor Seq where
        fmap f (Seq i xs) = Seq i (fmap f xs)
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




insertMessage :: (?configuration :: Configuration c, Eq m) => Maybe m -> [m] -> [m]
insertMessage Nothing ms = ms
insertMessage (Just m) ms 
        | m `elem` ms = ms
        | otherwise = take (memory ?configuration) $  m : ms


          
-- | the value of sequences  m : ms
type MF = Maybe Freq

data  Node a m = Node {
        roduction :: Seq (Maybe m), 
        store :: [m],
        load :: a,
        transmit :: Seq MF, -- ^ transmitting sequence
        receives :: [(Seq MF,Int)], -- ^ set of receiving sequences
        contract :: Seq Bool, -- ^ self spot sequence
        publicity :: Bool, -- ^ sync self spot condition
        collect :: Bool, -- ^ sequence harvesting
        chisentechi :: S.Set (Key,Key) 
        } deriving (Show)

-- | expose a node and a stepping , to permit node changing and query while hiding the stepping state
data Close a m = Close (Node a m) (Node a m -> Step a m)

inspect :: Close a m -> Node a m
inspect (Close n f) = n

-- | Stepping results. Any constructor result hold the new node. Receiving mode is completed with the received message
data Step a m   = Receive a Chan (Maybe (Seq MF,Maybe m) -> Close a m)  -- ^ receive mode , possibly receive a seq on chan
                | Transmit Chan (Seq MF,Maybe m) (Close a m)  -- ^ transmit mode, send a seq on chan
                | Sleep (Close  a m) -- ^ sleep mode , just step

-- | split a list of Step 
partitionStep :: [Step a m] -> ([((Chan,(Seq MF, Maybe m)),Close a m)],[(a,Chan,Maybe (Seq MF, Maybe m) -> Close a m)],[Close a m])
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
insertSeq :: Seq MF -> Key -> Node a m -> Node a m
insertSeq s@(Seq k1 _) k (Node hs ms a ts rss ps l q m) 
                | s `elem` (ts:map fst rss)     = Node hs ms a ts rss ps l q m'
                | otherwise                     = Node hs ms a ts ((s,-1) : rss) ps l q m'
        where m' = S.insert (k,k1) m

-- | Stepping state
data Cond = MustTransmit | MustReceive | UnMust

-- | close a node and its future
close :: (?configuration :: Configuration c, Eq m) => Cond -> Node a m -> Close a m
close c n = Close n (step c)

-- | specialized for unmust
closeU :: (?configuration :: Configuration c, Eq m) => Node a m -> Close a m
closeU = close UnMust


-- | Core logic. Node a fields are tested sequentially with pattern matching. First success fires the right Step
step :: (?configuration :: Configuration c, Eq m) => Cond -> Node a m -> Step  a m



-- Obliged to listen in the common chan after a publ
step MustReceive (Node (tailSeq -> hs) ms a (tailSeq -> ts) (stepReceivers -> rss) (tailSeq -> ps) l q chi) = Receive a Common f where
        f Nothing = closeU (Node hs ms a ts rss ps l q chi)  
        f (Just (s,Nothing)) = closeU (insertSeq s (key s) $ Node hs ms a ts rss ps l q chi)  
-- try to publ on a sync window on link channel
step MustTransmit (Node (tailSeq -> hs) ms a (tailSeq -> ts) (stepReceivers -> rss) (tailSeq -> ps) l q chi) = Transmit Common (ts,Nothing) . close MustReceive $ Node hs ms a ts rss ps l q chi

-- time to transmit: we transmit a receiving seq and roll the receiving seqs
step UnMust (Node (tailSeq -> hs) ms a (Seq i (Just c:ts)) (id &&& filter ((==0) . snd) -> (rss,[])) (tailSeq -> ps) l q chi)  
        = Transmit (Chan c) (Seq i ts,listToMaybe ms) . closeU $ Node hs (roll ms) a (Seq i ts) (roll $ stepReceivers rss) ps l q chi 
step UnMust (Node (tailSeq -> hs) ms a (Seq i (Just c:ts)) (head . filter ((==0) . snd) &&& roll -> (x,rss)) (tailSeq -> ps) l q chi) 
        = Transmit (Chan c) (tailSeq . fst $ x,listToMaybe ms) . closeU $ Node hs (roll ms) a (Seq i ts) (stepReceivers rss) ps l q chi
--  time to listen on a receiving seq
step UnMust (Node (tailSeq -> hs) ms a (tailSeq -> ts) 
                (select (isJust . head . core . fst) -> Just ((Seq i (Just c : xs),n),g)) 
                (tailSeq -> ps) l q chi) = Receive a (Chan c) f where
        new m k = Node hs (insertMessage m ms) a ts (stepReceivers $ g (Seq i (Just c : xs),k)) ps l q chi
        f Nothing = closeU $ new Nothing $ n - 1
        f (Just (s,m)) = closeU $ insertSeq s i. new m $ 0
-- time to transmit on common chan our transmit seq, setting the duty to listen right after
step UnMust (Node (Seq n (Just h:hs)) ms a (tailSeq -> ts) (stepReceivers -> rss) (tailSeq -> ps) l q chi) = Sleep . closeU $ 
         Node (Seq n hs) (insertMessage (Just h) ms) a ts rss ps l q chi

step UnMust (Node (tailSeq -> hs) ms a (tailSeq -> ts) (stepReceivers -> rss) (Seq i (True:ps)) l q chi) = Transmit Common (ts,Nothing) $ close MustReceive $ Node hs ms a ts rss (Seq i ps) l q chi
-- time to receive freely on common channel

step UnMust (Node (tailSeq -> hs) ms a (tailSeq -> ts) (stepReceivers -> rss) (tailSeq -> ps) l (id &&& (l ||) -> (q,True)) chi) = Receive a Common f where
        f Nothing = closeU $ Node hs ms a ts rss ps l q chi 
        f (Just (s,Nothing)) =  close (if l then MustTransmit else UnMust) $ (if q then insertSeq s (key s) else id) $ Node hs ms a ts rss ps l q chi 

-- time to sleep
step UnMust (Node hs ms a (tailSeq -> ts) (stepReceivers -> rss) (tailSeq -> ps) l q chi) = Sleep . closeU $ Node hs ms a ts rss ps l q chi

-- boot a node
mkStep :: (?configuration :: Configuration c,Eq m) => Node a m -> Step a m
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

mkSeqProd n freq x = Seq n [if y then Just x else Nothing | y <- core $ mkBoolSeq n freq] 
mkNode  :: (?configuration :: Configuration c) 
        => Int -- ^ node unique id
        -> a
        -> m  
        -> Node a m
mkNode ((*4) -> n) p x = Node 
        (mkSeqProd (n + 3) (rodfrequency ?configuration) x)
        []
        p
        (mkSeq n (numchannels ?configuration) (txfrequency ?configuration))
        []
        (mkBoolSeq (n + 2) (pufrequency ?configuration))
        True
        True
        S.empty



-- | eliminate unresponsive sequences
forget :: Int -> Node a m -> Node a m
forget n (Node hs ms a ts rss ps l q chi) = Node hs ms a ts (filter ((> negate n) . snd) rss)  ps l q chi


-- | partition listeners between node listener and unlistener 
partitionChiSenteChi :: Node a m -> ([Key], [Key])
partitionChiSenteChi (Node _ _ a s _ _ _ _ chi) = (map fst . S.toList *** map fst . S.toList) . S.partition ((== key s) . snd) $ chi

test :: (a -> b) -> a -> (a,b)
test f = id &&& f

-- | empty the chisentechi bin
resetChiSenteChi :: Node a m -> Node a m
resetChiSenteChi n = n{chisentechi = S.empty}

every j i = i `mod` j == 0

modNode (every 10 -> True) n = resetChiSenteChi n
modNode _ n@(Node hs ms a ts rss ps _ _ w) = forget 5 $ Node hs ms a ts rss ps ((<2) . length . fst . partitionChiSenteChi $ n) ((<2) . length $ rss) w


data World a = World Int [a] deriving (Show)

elems (World i xs) = xs

instance Functor World where
        f `fmap` World i x = World i (f `fmap` x)
       

stepWorld :: (a -> a -> Bool) -> (Int -> Node a m -> Node a m) -> World (Close a m) -> World (Close a m)
stepWorld ckd v (World n cs) = let 
        xs = map (\(Close no f) -> f(v n no)) cs -- :: [Step Pos m] 
        (ts,rs,ss) = partitionStep xs 
        ms t = map (fst . head) . groupBy ((==) `on` (fst . fst)) . sortBy (comparing $ fst . fst) $
                 filter (ckd t . load . inspect . snd) $ ts -- :: [(Chan,Seq MF)]
        f (t,c,g) = g $ lookup c (ms t)
        nrs = map f rs
        in World (n + 1) $ map snd ts ++ nrs ++ ss

       

{-
-- | A list of Node as with a 2d pos

type Pos = (Float,Float)

(x1,x2) -. (y1,y2) = (x1-y1,x2-y2)
(x,y) ^-^ 2 = x ^ 2 + y ^ 2

distance r px py = (sqrt $ (px -. py) ^-^ 2) < r

-}
