module Network.QUIC.Internal.Frame.Stream where
import Data.Word
import Data.Bits
import Data.ByteString.Lazy(toStrict)
import qualified Data.ByteString as BS 
import qualified Data.ByteString.Lazy as BSL
import Data.Binary.Get
import Data.Binary.Put

import qualified Network.QUIC.Error as E
import Network.QUIC.Internal
import Network.QUIC.Internal.Util.Binary

data StreamFrame = StreamFrame { streamHasFin :: !Bool
                     , streamHasDataLenField :: !Bool
                     , streamOffset :: !Int
                     , streamStreamId :: !Int
                     , streamStreamData :: !BS.ByteString
                     } deriving (Show, Eq)

decodeStreamFrame :: BSL.ByteString -> E.QUICResult (StreamFrame, BSL.ByteString)
decodeStreamFrame  bs = case runGetOrFail  get' $ BSL.tail bs of
                                                             Right (bs, _, frame) -> Right (frame, bs)
                                                             Left _ -> Left E.InvalidFrameData
  where
    b = BSL.head bs
    get' = get  (wordHasStreamFin b)  
                (wordHasStreamDataLenField b)
                (word2streamOffsetSize b)  
                (word2streamStreamIdSize b)

    get :: Bool -> 
           Bool -> 
           Int -> 
           Int ->
           Get StreamFrame
    get  fin hasDataLen offsetSize streamIDSize = do
        streamID <- getStreamID streamIDSize
        offset <- getIntNbyte offsetSize
        dataLen <- if hasDataLen then getInt2byte else return 0
        streamData <- getData dataLen
        return $ StreamFrame fin hasDataLen offset streamID streamData

    getData :: Int -> Get BS.ByteString
    getData 0 = toStrict <$> getRemainingLazyByteString 
    getData n = getByteString $ fromIntegral n



wordHasStreamFin :: Word8 -> Bool
wordHasStreamFin i = i .&. 0x40  == 0x40

wordHasStreamDataLenField :: Word8 -> Bool
wordHasStreamDataLenField i = i .&. 0x20 == 0x20


word2streamOffsetSize :: Word8 -> Int
word2streamOffsetSize i = case i .&. 0x1c of
                        0x00 -> 0
                        0x04 -> 1
                        0x08 -> 2
                        0x0c -> 3
                        0x10 -> 4
                        0x14 -> 6
                        0x18 -> 7
                        0x1c -> 8
                        _   -> -1

word2streamStreamIdSize :: Word8 -> Int
word2streamStreamIdSize i = case i .&. 0x03 of
                       0x00 -> 1
                       0x01 -> 2
                       0x02 -> 4
                       0x03 -> 8
                       _    -> 0

encodeStreamFrame :: StreamFrame -> BS.ByteString
encodeStreamFrame (StreamFrame hasFin hasDataLen offset streamID streamData) = undefined

