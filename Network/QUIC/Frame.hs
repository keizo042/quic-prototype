module Network.QUIC.Frame
  (
      FrameTypes(..)
    , Frame(..)
    , int2FrameTypes
  ) where
import           Data.Bits
import           Data.ByteString.Lazy
import           Network.QUIC.Error   (ErrorCodes (..))

data FrameTypes  = STREAM { fin :: Bool,  dataLength :: Bool, id:: Int}
                 | ACK{frame :: Bool, largestacked :: Int, blockLength :: Int }
                 | PADDING
                 | RST_STREAM
                 | CONNECTION_CLOSE
                 | GOAWAY
                 | WINDOW_UPDATE
                 | BLOCKED
                 | STOP_WAITING
                 | PING
                 | Undefined
                 deriving (Show, Eq)

s2frametype i =  STREAM fin len conId
  where
      fin = (i .&. 0x40 ) == 0x40
      len = (i .&. 0x1c ) == 0x1c
      conId = (case ( i .&. 0x03) of
                   0x00 -> 8
                   0x01 -> 16
                   0x02 -> 32
                   0x03 -> 64
                   _    -> 0)


ack2frametype i = ACK frame l  bl
  where
    frame = undefined
    l = undefined
    bl = undefined

int2FrameTypes :: Int -> FrameTypes
int2FrameTypes i
  | ( i .&. 0x80 ) == 0x80 =  s2frametype i
  | ( i .&. 0x40 ) == 0x40 =  ack2frametype i
  | otherwise = i2f i
  where
    i2f 0x00 = PADDING
    i2f 0x01 = RST_STREAM
    i2f 0x02 = CONNECTION_CLOSE
    i2f 0x03 = GOAWAY
    i2f 0x04 = WINDOW_UPDATE
    i2f 0x05 = BLOCKED
    i2f 0x06 = STOP_WAITING
    i2f 0x07 = PING
    i2f _    = Network.QUIC.Frame.Undefined

data AckBlock = AckBlock { numberBlocks        :: Int
                         , firstAckBlockLength :: Int
                         , gapToNextBlock      :: Int
                         , ackBlockLength      :: Int
                         }deriving Show

data AckTimeStamp = AckTimeStamp { deltaLargestAcked  :: Int
                                 , timeSincePrevAcked :: Int
                                 } deriving Show

data Frame = Stream  { streamStreamId   :: Int
                     , streamOffSet     :: Int
                     , streamDataLength :: Int
                     , streamStreamData :: ByteString
                     }
           | Ack  { largestAcked         :: Int
                  , ackDelay             :: Int
                  , ackBlock             :: [AckBlock]
                  , ackNumTimeStamps     :: Int
                  , ackDeltaLargestAcked :: Maybe Int
                  , timeLargestAcked     :: Maybe Int
                  , ackTimeStamps        :: Maybe [AckTimeStamp]
                  }
           | StopWaiting  { leastUnackedDelta :: Int }
           | WindowUpdate { windowUpdateStreamId   :: Int
                          , windowUpdateByteOffset :: Int
                          }
           | Blocked { blockedStreamId :: Int}
           | RstStream  { rstStreamStreamId   :: Int
                        , rstStreamByteOffset :: Int
                        , rstStreamErrCode    :: ErrorCodes
                        }
           | Padding
           | Ping
           | ConnectionClose  { connClosedErrorCode :: ErrorCodes
                              , reasonPahse         :: ByteString
                              }
           | Goaway { goAwayErrorCode        :: ErrorCodes
                    , goAwayLastGoodStreamId :: Int
                    , goAwayReasonPhase      :: ByteString
                    }
           deriving (Show)
