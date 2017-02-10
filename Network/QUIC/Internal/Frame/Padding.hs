module Network.QUIC.Internal.Frame.Padding where
import Network.QUIC.Error
import Data.Binary.Get
import Data.Binary.Put


data PaddingFrame = PaddingFrame

decodeFramePing :: ByteString -> QUICResult (PaddingFrame, ByteString)
decodeFramePing bs = Right (Ping, BSL.empty)

encodeFramePing :: PaddingFrame -> ByteString
encodeFramePing = undefined
