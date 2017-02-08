module Network.QUIC.Internal.Frame.Blocked where
import Network.QUIC.Error
import Data.Binary.Get
import Data.Binary.Put

data BlockedFrame = BlockedFrame { blockedStreamId :: Int } deriving Show

decodeFrameBlocked :: Settings -> ByteString -> QUICResult (Frame, ByteString)
decodeFrameBlocked s bs = case BG.runGetOrFail get bs of
                               Right (bs, _, frame) -> Right (frame, bs)
                               Left _               -> Left Error.InvalidFrameData
  where
    get :: BG.Get Frame
    get = Blocked <$> BG.getInt32
