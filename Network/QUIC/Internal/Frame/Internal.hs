module Network.QUIC.Internal.Frame.Stream 
  (
       isStreamFrame
    , isAckFrame
  )where
import Data.ByteString.Lazy
import           Data.Int
import qualified Data.Word            as Word
import qualified Network.QUIC.Error   as Error
import Network.QUIC.Types

class Frame a where
    toFrame :: ByteString -> ByteSize -> a
    fromFrame :: a - > ByteSize -> ByteString

isStreamFrame :: Word8 -> Bool
isStreamFrame i = i .&. 0x80 == 0x80

isAckFrame :: Word8 -> Bool
isAckFrame i = i .&. 0x40 == 0x40

