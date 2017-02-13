module Network.QUIC.Internal.Frame.StopWaiting where

import qualified Network.QUIC.Error as E
import Network.QUIC.Types

import Data.Binary.Get
import Data.Binary.Put

import Data.ByteString.Lazy
import Network.QUIC.Internal.Util.Binary

data StopWaitingFrame = StopWaitingFrame { stopWaitingLeastUnackedDelta :: Int }
  deriving (Show, Eq)

decodeFrameStopWaiting ::  ByteString -> ByteSize -> E.QUICResult (StopWaitingFrame, ByteString)
decodeFrameStopWaiting bs n =  case runGetOrFail get bs of
                               Right (bs, _, frame) -> Right (frame, bs)
                               Left _               -> Left E.InvalidStopWaitingData
  where
    get :: Get StopWaitingFrame
    get = StopWaitingFrame <$> (getIntNbyte n)

encodeStopWaitingFrame :: StopWaitingFrame -> ByteString
encodeStopWaitingFrame (StopWaitingFrame delta) = runPut put 
  where
    put = undefined
    countSize :: Int -> ByteSize
    countSize = undefined
