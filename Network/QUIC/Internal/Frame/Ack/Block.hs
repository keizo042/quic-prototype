module Network.QUIC.Internal.Frame.Ack.Block where
import qualified Network.QUIC.Error as E

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL


data AckBlock = AckBlock { ackBlockGap :: Maybe Int
                         , ackBlockLength      :: Int
                         } deriving Show

decodeAckBlock :: BSL.ByteString -> E.QUICResult (AckBlock, BSL.ByteString)
decodeAckBlock = undefined
