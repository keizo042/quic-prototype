module Network.QUIC.Internal.Frame.StopWaiting where
import Network.QUIC.Error
import Data.Binary.Get
import Data.Binary.Put

data StopWaitingFrame = StopWaitingFrame { stopWaitingLeastUnackedDelta :: Int }
  deriving (Show, Eq)

decodeFrameStopWaiting :: Settings -> ByteString -> QUICResult (Frame, ByteString)
decodeFrameStopWaiting s bs =  case BG.runGetOrFail (get $ packetNumberSize s) bs of
                               Right (bs, _, frame) -> Right (frame, bs)
                               Left _               -> Left Error.InvalidStopWaitingData
  where
    get :: Int -> BG.Get Frame 
    get n = StopWaiting <$> (BG.getIntN n)
