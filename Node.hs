{-# LANGUAGE ViewPatterns, ImplicitParams, DeriveDataTypeable, TemplateHaskell, DeriveFunctor #-}
module Node where
import Control.Lens
import qualified Data.Set as S
import Control.Arrow (second, first)
import Timed
import Seq

-- | Configuration of a node. 
data Configuration = Configuration {
        txfrequency :: Int, -- ^ transmissions frequency
        pufrequency :: Int, -- ^ pubblicity frequency
        rodfrequency :: Int, -- ^ boh
        memory :: Int, -- ^ number of remembered messages
        numchannels :: Int, -- ^ channel spectrum 
        lmessagettl :: Int -- ^ message duration in memory
        } deriving (Show)-- , Typeable, Data)


-- | A radio channel
type Freq = Int

-- | Separation by construction of the link channel from the data channels
data Chan = Common | Chan Freq deriving (Eq, Ord)


type SeqT = SeqM Freq
data Neighbor = Neighbor {
        _transmissions :: SeqT,
        _misseds :: Int
        }

$(makeLenses ''Neighbor)

-- | Node: a is the semantic, m is the message
data  Node m = Node {
        _roduction :: SeqM m, 
        _store :: [Timed m], -- ^ memory of received messages
        _transmit :: SeqT,  -- ^ transmitting sequence
        _receives :: [Neighbor] , -- ^ set of receiving sequences (WHY Int?)
        _contract :: Seq Bool, -- ^ self spot sequence
        _publicity :: Bool, -- ^ sync self spot condition
        _collect :: Bool, -- ^ sequence harvesting
        _chisentechi :: S.Set (Key,Key) 
        } 

$(makeLenses ''Node)

-- | communication must have a transmitting sequence and can have a message
data Comm m = Comm SeqT (Maybe (Timed m))

-- | Stepping results. Any constructor result hold the new node. Receiving mode is completed with the received message
data Step  m   = Receive  Chan (Maybe (Comm m) -> Close  m)  -- ^ receive mode , possibly receive a Comm on chan
                | Transmit Chan (Comm m) (Close  m)  -- ^ transmit mode, send a seq on chan
                | Sleep (Close  m) -- ^ sleep mode , just step

-- | expose a node and a stepping , to permit node changing and query while hiding the stepping state
data Close  m = Close {
        _node :: Node  m,
        _future :: Node  m -> Step  m
        }

$(makeLenses ''Close)

