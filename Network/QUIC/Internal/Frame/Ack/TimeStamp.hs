module Network.QUIC.Internal.Frame.Ack.TimeStamp where
import Network.QUIC.Error
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString.Lazy


data AckTimeStamp = AckTimeStamp { deltaLargestAcked  :: Int
                                 , timeSincePrevAcked :: Int
                                 } deriving Show

decodeAckTimeStamp :: ByteString -> QUICResult (AckTimeStamp, ByteString)
decodeAckTimeStamp = undefined

encodeAckTimeStamp :: AckTimeStamp -> ByteString
encodeAckTimeStamp = undefined

