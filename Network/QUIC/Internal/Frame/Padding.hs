module Network.QUIC.Internal.Frame.Padding where
import Network.QUIC.Error
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString.Lazy


data PaddingFrame = PaddingFrame
                    deriving (Show,Eq)

decodeFramePadding :: ByteString -> QUICResult (PaddingFrame, ByteString)
decodeFramePadding bs = Right (PaddingFrame, empty)

encodeFramePadding :: PaddingFrame -> ByteString
encodeFramePadding PaddingFrame = runPut $ putWord8 0x00
