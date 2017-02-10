module Network.QUIC.Internal.Frame.Ack where
import Data.Binary.Get
import Data.Binary.Put
import Data.Word

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS

import qualified Network.QUIC.Error as E

import Network.QUIC.Internal
import Network.QUIC.Internal.Frame.Ack.Block
import Network.QUIC.Internal.Frame.Ack.TimeStamp


data AckFrame = AckFrame { largestAcked :: Int
                         , lowestAcked :: Int
                         , blocks :: [AckBlock]
                         , receivedTime :: Int
                         , delay :: Int
                         } deriving Show
                         
decodeAckFrame ::  BSL.ByteString -> E.QUICResult (AckFrame, BSL.ByteString)
decodeAckFrame bs = case runGetOrFail get bs of
                      Right (bs, _, frame)  -> Right (frame, bs)
                      Left _        -> Left E.InvalidAckData
  where
    get  :: Get AckFrame
    get =  undefined
