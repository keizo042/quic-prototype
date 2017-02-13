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
import Data.Bits

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS

import qualified Network.QUIC.Error as E

import Network.QUIC.Internal
import Network.QUIC.Internal.Frame.Ack.Block
import Network.QUIC.Internal.Frame.Ack.TimeStamp


data AckFrame = AckFrame { largestAcked :: Int
                         , lowestAcked :: Int
                         , blocks :: [AckBlock]
                         , receivedTime :: Int
                         , delayTime :: Int
                         } deriving Show
                         

getAckedLen = undefined

getDelayTime = undefined

decodeAckFrame ::  BSL.ByteString -> E.QUICResult (AckFrame, BSL.ByteString)
decodeAckFrame bs = case runGetOrFail get bs of
                      Right (b, _, frame)  -> case frame of 
                                                   Right f -> Right (f, b)
                                                   Left e ->  Left e
                      Left _        -> Left E.InvalidAckData
  where
    get  :: Get (E.QUICResult AckFrame)
    get =  do
      f <- getWord8
      ackedLen <- getAckedLen (ackedLenSize f)
      delay <- getDelayTime 
      numBlockLen <- if (hasRange f) then  undefined else  return 0
      return $ Right undefined
        where
          hasRange :: Word8 -> Bool
          hasRange = undefined
          ackedLenSize :: Word8 -> Int
          ackedLenSize = undefined


encodeAckFrame :: AckFrame -> BSL.ByteString
encodeAckFrame a = runPut $ put a
  where
    put :: AckFrame -> Put
    put frame = undefined

    flags :: Word8
    flags = 0x40
      .|. 0x0
