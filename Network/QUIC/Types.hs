module Network.QUIC.Types
  (
    Settings(..),
    Nonce,
    Version(..),
    ver2int,
    int2ver,
    Tags,
  )
  where
import Network.QUIC.Stream 

data Settings = Settings { idleTimeout :: Int
                         , mtuSize :: Int
                         , mspc :: Int
                         , packetNumberSize :: Int
                         , peer :: Peer
                         , endpoint :: EndPoint
    } deriving Show

type Nonce = Int

data Version = Q033
             | Q034
             | Q035
             | Q036
             | Unsuppoted
             deriving (Show, Eq)

ver2int :: Version -> Int
ver2int Q033       = 33
ver2int Q034       = 34
ver2int Q035       = 35
ver2int Q036       = 36
ver2int Unsuppoted = 0

int2ver :: Int -> Version
int2ver 33 = Q033
int2ver 34 = Q034
int2ver 35 = Q035
int2ver 36 = Q036
int2ver _  = Unsuppoted

data Tags = Tags
