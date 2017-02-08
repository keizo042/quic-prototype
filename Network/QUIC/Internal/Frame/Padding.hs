module Network.QUIC.Internal.Frame.Padding where
import Network.QUIC.Error
import Data.Binary.Get
import Data.Binary.Put


data PaddingFrame = PaddingFrame

decodeFramePing :: Settings -> ByteString -> QUICResult (Frame, ByteString)
decodeFramePing s bs = Right (Ping, BSL.empty)
