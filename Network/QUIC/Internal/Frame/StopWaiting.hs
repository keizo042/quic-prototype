module Network.QUIC.Internal.Frame.StopWaiting where

import qualified Network.QUIC.Error as E

import Data.Binary.Get
import Data.Binary.Put

import Data.ByteString.Lazy
import Network.QUIC.Internal.Util.Binary

data StopWaitingFrame = StopWaitingFrame { stopWaitingLeastUnackedDelta :: Int }
  deriving (Show, Eq)

decodeFrameStopWaiting ::  ByteString -> E.QUICResult (StopWaitingFrame, ByteString)
decodeFrameStopWaiting bs =  case runGetOrFail (get n)bs of
                               Right (bs, _, frame) -> Right (frame, bs)
                               Left _               -> Left E.InvalidStopWaitingData
  where
    n = 4
    get :: Int -> Get StopWaitingFrame
    get n = StopWaitingFrame <$> (getIntNbyte n)

encodeStopWaitingFrame :: StopWaitingFrame -> ByteString
encodeStopWaitingFrame = undefined
