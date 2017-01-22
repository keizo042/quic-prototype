module Network.QUIC.Frame
  (
      FrameType(..)
    , Frame(..)
    , AckBlock(..)
    , AckTimeStamp(..)
    , word82FrameType
  ) where
import Data.Word
import           Data.Bits
import           Data.ByteString.Lazy
import           Network.QUIC.Error   (ErrorCodes (..))
import qualified Network.QUIC.Header as H

data FrameType  = STREAM { fin :: Bool,  dataLength :: Bool, offset :: H.ByteSize, id:: Int}
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




word82FrameType :: Word8 -> FrameType
word82FrameType i
  | i .&. 0x80 == 0x80 =  s2frametype i
  | i .&. 0x40 == 0x40 =  ack2frametype i
  | otherwise = i2f i
  where

    s2frametype :: Word8 -> FrameType
    s2frametype i =  STREAM fin len H.Byte6 conId
      where
          fin = i .&. 0x40  == 0x40
          len = i .&. 0x1c  == 0x1c
          conId = case i .&. 0x03 of
                       0x00 -> 8
                       0x01 -> 16
                       0x02 -> 32
                       0x03 -> 64
                       _    -> 0
    ack2frametype :: Word8 -> FrameType
    ack2frametype i = ACK frame l  bl
      where
        frame = undefined
        l = undefined
        bl = undefined

    i2f :: Word8 -> FrameType
    i2f 0x00 = PADDING
    i2f 0x01 = RST_STREAM
    i2f 0x02 = CONNECTION_CLOSE
    i2f 0x03 = GOAWAY
    i2f 0x04 = WINDOW_UPDATE
    i2f 0x05 = BLOCKED
    i2f 0x06 = STOP_WAITING
    i2f 0x07 = PING
    i2f _    = Network.QUIC.Frame.Undefined

data AckBlock = AckBlock { ackBlocknumberBlocks        :: Int
                         , ackBlockfirstAckBlockLength :: Int
                         , ackBlockgapToNextBlock      :: Int
                         , ackBlockLength      :: Int
                         } deriving Show

data AckTimeStamp = AckTimeStamp { deltaLargestAcked  :: Int
                                 , timeSincePrevAcked :: Int
                                 } deriving Show

data Frame = Stream  { streamFrameType :: FrameType
                     , streamStreamId   :: Int
                     , streamOffSet     :: Int
                     , streamDataLength :: Int
                     , streamStreamData :: ByteString
                     }
           | Ack  {  ackFrameType :: Int
                  , ackLargestAcked         :: Int
                  , ackDelay             :: Int
                  , ackBlock             :: [AckBlock]
                  , ackNumTimeStamps     :: Int
                  , ackDeltaLargestAcked :: Maybe Int
                  , ackTimeLargestAcked     :: Maybe Int
                  , ackTimeStamps        :: Maybe [AckTimeStamp]
                  }
           | StopWaiting  { sstopWaitingLeastUnackedDelta :: Int }
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
                              , connClosedreasonPahse         :: ByteString
                              }
           | Goaway { goAwayErrorCode        :: ErrorCodes
                    , goAwayLastGoodStreamId :: Int
                    , goAwayReasonPhase      :: ByteString
                    }
           | FrameError { frameError :: ErrorCodes }
           deriving Show
