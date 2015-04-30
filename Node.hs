{-# LANGUAGE ViewPatterns, ImplicitParams, DeriveDataTypeable, TemplateHaskell, DeriveFunctor, DataKinds, KindSignatures, GADTs, PolyKinds #-}

module Node where
import Control.Lens
import Control.Arrow (second, first)
import Data.List (partition, delete, find)
import Timed
import Seq

-- | NodeConfiguration of a node. 
data NodeConfiguration = NodeConfiguration {
        txfrequency :: Int, -- ^ transmissions frequency
        pufrequency :: Int, -- ^ pubblicity frequency
        rodfrequency :: Int, -- ^ node self message frequency
        droplevel :: Int, -- ^ max number of consecutive missed listenings before dropping
        alertlevel :: Int, -- ^ minimum number of listeners to stop using sync listening window on common channel
        neighborlevel :: Int, -- ^ minimum number of neighbors to stop listening on common channel
        neighbormemory :: Int, -- ^ maximum number of neighbor
        memory :: Int, -- ^ number of remembered messages
        numchannels :: Int, -- ^ channel spectrum 
        lmessagettl :: Int -- ^ message duration in memory
        } deriving (Show)-- , Typeable, Data)


-- | A radio channel
type Freq = Int

-- | Promoted channel kinds
data TChan = TCommon | TFree

-- | Separation by construction of the link channel from the data channels
data Chan (t :: TChan) where
        Common :: Chan TCommon
        Free :: Freq -> Chan TFree

instance Eq (Chan t) where
        Common == Common = True
        Free n == Free m = n == m
        _ == _ = False

-- | sequence specialized for frequencies
type SeqT = SeqM Freq

-- | vision of a neighbor node 
data Neighbor = Neighbor {
        _transmissions :: SeqT, -- ^ neighbor transmission sequence
        _misseds :: Int -- ^ count of missed listenings (out of reach), very improbable idea to bring to hardware
        }

makeLenses ''Neighbor

-- | check if a neighbor is still interesting
keepNeighbor :: (?nconf :: NodeConfiguration) => Neighbor -> Bool
keepNeighbor x = view misseds x < droplevel ?nconf

-- | let heads go from neighbors
tailNeighbors :: [Neighbor] -> [Neighbor]
tailNeighbors = map (over transmissions tailSeq)

-- | Node m is the message type
data  Node m = Node {
        _production :: SeqM m, -- ^ fake random self message
        _messages :: [Timed m], -- ^ memory of received messages
        _transmit :: SeqT,  -- ^ transmitting sequence 
        _neighbors :: [Neighbor] , -- ^ set of receiving sequences 
        _contract :: Seq Bool, -- ^ self spot sequence
        _listeners :: [Key] -- ^ listeners
        } 

makeLenses ''Node


-- | starving for listeners 
shouldSync :: (?nconf :: NodeConfiguration) => Node m -> Bool
shouldSync n = length (view listeners n) < alertlevel ?nconf

-- | starving for receivers
shouldListen ::  (?nconf :: NodeConfiguration) => Node m -> Bool
shouldListen n = length (view neighbors n) < neighborlevel ?nconf

-- | eliminate lost neighbors even from listeners (based on simmetric signal property)
clean ::(?nconf :: NodeConfiguration) => Node m -> Node m
clean n = let
        (xs,ys) = partition keepNeighbor $ view neighbors  n
        ls = filter (not . (`elem` map (view $ transmissions . key) ys)) $ view listeners n
        in set listeners ls . set neighbors xs $ n 

-- | insert a add neighbor if the sequence key is unknown
addNeighbor' :: (?nconf :: NodeConfiguration) => Neighbor -> Node  m -> Node m
addNeighbor' n = over neighbors $ take (neighbormemory ?nconf) <$>
        (maybe . (n:) <*> const  <*> find ((== view (transmissions . key) n) . view (transmissions . key)))

addNeighbor :: (?nconf :: NodeConfiguration) => SeqT -> Node  m -> Node m
addNeighbor s = addNeighbor' (Neighbor s 0)

addNeighborOrListener :: (?nconf :: NodeConfiguration) => SeqT -> Node  m -> Node m
addNeighborOrListener s n 
	| view key s == view (transmit . key) n = addListener s n
	| otherwise = addNeighbor' (Neighbor s 1) n 

-- | insert a add listener
addListener :: SeqT -> Node  m -> Node m
addListener (Seq k _) = over listeners ((k:) . delete k) 

-- insert a message if not present
addMessage :: (?nconf :: NodeConfiguration, Eq m) => Timed m -> Node m -> Node m
addMessage m = over messages f where
        f ms 
          | any ((view message m ==) . view message) $ ms = ms  
          | otherwise                                     = take (memory ?nconf) $  m : ms 

-- | communication must have a transmitting sequence and can have a message
data Comm  (t :: TChan)  m where
        Publ :: SeqT -> Comm TCommon m
        Info :: SeqT -> Maybe (Timed m) -> Comm TFree m

-- | Node possible states. Any state hold the add node as a Future. Receiving node is determined after the received message
data State  m where
                ReceiveCommon  :: Chan TCommon -> (Maybe (Comm TCommon m) -> Future m) -> State m  -- ^ receive mode , possibly receive a Comm on chan
                ReceiveFree  :: Chan TFree -> (Maybe (Comm TFree m) -> Future m) -> State m  -- ^ receive mode , possibly receive a Comm on chan
                TransmitCommon :: Chan TCommon -> Comm TCommon m -> Future m -> State m -- ^ transmit mode, send a seq on chan
                TransmitFree :: Chan TFree -> Comm TFree m -> Future m -> State m -- ^ transmit mode, send a seq on chan
                Sleep :: Future m -> State m -- ^ sleep mode , just step

-- | expose a node and a step to a add state , to permit node changing and query while closing the stepping operation
data Future  m = Future {
        _node :: Node  m,
        _future :: Node  m -> State m
        }

makeLenses ''Future

-- | proceed from a node to its next state
applyFuture :: Future m -> State m
applyFuture (Future m f) = f m

mkNode  :: (?nconf :: NodeConfiguration) 
        => Key -- ^ node unique id
        -> m  -- ^ fixed node message
        -> (Key, Node m)
mkNode ((*3) -> n) x = (n + 4, Node 
        (mkSeqProd (n + 2) (rodfrequency ?nconf) x)
        []  --messages
        (mkSeq n (numchannels ?nconf) (txfrequency ?nconf))
        [] -- receivers
        (mkBoolSeq (n + 1) (pufrequency ?nconf))
        []
        )

