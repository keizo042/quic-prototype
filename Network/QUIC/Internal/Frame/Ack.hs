module Network.QUIC.Internal.Frame.Ack 
  (
      AckFrame
    , encodeAckFrame
    , decodeAckFrame
  )where
import Data.Binary.Get
import Data.Binary.Put
import Data.Word
import Data.Time
import Data.UnixTime
import Data.Bits

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS

import qualified Network.QUIC.Error as E
import Network.QUIC.Types

import Network.QUIC.Internal
import Network.QUIC.Internal.Util.Binary
import Network.QUIC.Internal.Frame.Ack.Block
import Network.QUIC.Internal.Frame.Ack.TimeStamp


data AckFrame = AckFrame { largestAcked :: PacketNumber
                         , lowestAcked :: PacketNumber
                         , blocks :: [AckBlock]
                         , receivedTime :: Int
                         , delayTime :: Int
                         } deriving Show 
                         


decodeAckFrame ::  BSL.ByteString -> ByteSize -> E.QUICResult (AckFrame, BSL.ByteString)
decodeAckFrame bs n = case runGetOrFail get bs of
                      Right (b, _, frame)  -> case frame of 
                                                   Right f -> Right (f, b)
                                                   Left e ->  Left e
                      Left _        -> Left E.InvalidAckData
  where
    get  :: Get (E.QUICResult AckFrame)
    get =  do
      f <- getWord8
      largest <- getPacketNumber n
      delay <- getInt2byte
      blocks <- if (hasRange f) then  undefined else  return []
      return (Right $ AckFrame largest largest blocks t delay)
        where
          hasRange b = b .&. 0x20 == 0x20
          ackedRangeByteSize b = b .&. 0x0c == 0x0c
          ackBlockByteSize b = b .&. 0x03 == 0x03
          t = undefined


encodeAckFrame :: AckFrame -> BSL.ByteString
encodeAckFrame (AckFrame largest lowest blocks recived delay)  = runPut put 
  where
    put = do
      putWord8 b
    b :: Word8
    b = 0x40
