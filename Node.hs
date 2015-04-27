{-# LANGUAGE ViewPatterns, ImplicitParams, DeriveDataTypeable, TemplateHaskell, DeriveFunctor #-}
module Node where
import Control.Lens
import Control.Arrow (second, first)
import Data.List (partition)
import Timed
import Seq

-- | Configuration of a node. 
data Configuration = Configuration {
        txfrequency :: Int, -- ^ transmissions frequency
        pufrequency :: Int, -- ^ pubblicity frequency
        rodfrequency :: Int, -- ^ node self message frequency
        droplevel :: Int, -- ^ max number of consecutive missed listenings before dropping
        alertlevel :: Int, -- ^ minimum number of listeners to stop using sync listening window on common channel
        memory :: Int, -- ^ number of remembered messages
        numchannels :: Int, -- ^ channel spectrum 
        lmessagettl :: Int -- ^ message duration in memory
        } deriving (Show)-- , Typeable, Data)


-- | A radio channel
type Freq = Int

-- | Separation by construction of the link channel from the data channels
data Chan = Common | Chan Freq deriving (Eq, Ord)

-- | sequence specialized for frequencies
type SeqT = SeqM Freq

-- | vision of a neighbor node 
data Neighbor = Neighbor {
        _transmissions :: SeqT, -- ^ neighbor transmission sequence
        _misseds :: Int -- ^ count of missed listenings (out of reach)
        }

makeLenses ''Neighbor

-- | check if a neighbor is still interesting
keepNeighbor :: (?configuration :: Configuration) => Neighbor -> Bool
keepNeighbor x = view misseds x < droplevel ?configuration




-- | Node m is the message type
data  Node m = Node {
        _production :: SeqM m, -- ^ fake random self message
        _store :: [Timed m], -- ^ memory of received messages
        _transmit :: SeqT,  -- ^ transmitting sequence 
        _receives :: [Neighbor] , -- ^ set of receiving sequences 
        _contract :: Seq Bool, -- ^ self spot sequence
        _listeners :: [Key] -- ^ listeners
        } 

makeLenses ''Node

-- | let heads go from receives
tailReceives :: [Neighbor] -> [Neighbor]
tailReceives = map (over transmissions tailSeq)

-- | starving for listeners 
shouldSync :: (?configuration :: Configuration) => Node m -> Bool
shouldSync n = length (view listeners n) < alertlevel ?configuration

-- | starving for receivers
shouldListen ::  (?configuration :: Configuration) => Node m -> Bool
shouldListen n = length (view receives n) < alertlevel ?configuration

-- | eliminate lost neighbors even from listeners
clean ::(?configuration :: Configuration) => Node m -> Node m
clean n = let
        (xs,ys) = partition keepNeighbor $ view receives  n
        ls = filter (not . (`elem` map (view $ transmissions . key) xs)) $ view listeners n
        in set listeners ls . set receives ys $ n 
{-
-- | insert a new receiver in a node. The insertion in receivers happens if the sequence key is unknown.The chisentechi is updated always.
insertReceiver :: SeqT -> Node  m -> Node  m
insertReceiver s k n 
                | elemBy key s (view transmit n : map  (view transmissions) (view neighbor n)) = n  -- checks it's us or a known 
                | otherwise                     = over neighbor (Neighbor s 1:) n' -- a new unreceived friend
        where n' = over  (S.insert (k, view key s)) n 
-}

-- | communication must have a transmitting sequence and can have a message
data Comm m = Comm SeqT (Maybe (Timed m))

-- | Stepping results. Any constructor result hold the new node. Receiving mode is completed with the received message
data Step  m    = Receive  Chan (Maybe (Comm m) -> Close  m)  -- ^ receive mode , possibly receive a Comm on chan
                | Transmit Chan (Comm m) (Close  m)  -- ^ transmit mode, send a seq on chan
                | Sleep (Close  m) -- ^ sleep mode , just step

-- | expose a node and a stepping , to permit node changing and query while hiding the stepping state
data Close  m = Close {
        _node :: Node  m,
        _future :: Node  m -> Step  m
        }

$(makeLenses ''Close)

