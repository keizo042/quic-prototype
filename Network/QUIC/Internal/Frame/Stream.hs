module Network.QUIC.Internal.Frame.Stream 
  (
      StreamFrame(..)
    , encodeStreamFrame
    , decodeStreamFrame
  )where
import Data.Word
import Data.Bits
import Data.ByteString.Lazy(toStrict)
import qualified Data.ByteString as BS 
import qualified Data.ByteString.Lazy as BSL
import Data.Binary.Get
import Data.Binary.Put

import qualified Network.QUIC.Error as E
import Network.QUIC.Types
import Network.QUIC.Internal
import Network.QUIC.Internal.Util.Binary

data StreamFrame = StreamFrame { streamHasFin :: !Bool
                     , streamHasDataLenField :: !Bool
                     , streamOffset :: !Offset
                     , streamStreamId :: !Int
                     , streamStreamData :: !BS.ByteString
                     } deriving (Show, Eq)

decodeStreamFrame :: BSL.ByteString -> E.QUICResult (StreamFrame, BSL.ByteString)
decodeStreamFrame  bs = case runGetOrFail  get bs of
                                                             Right (b, _, frame) -> case frame of
                                                                                          Right f     -> Right (f, b)
                                                                                          (Left e)  -> Left e
                                                             Left _ -> Left E.InvalidFrameData
  where

    get :: Get (E.QUICResult StreamFrame)
    get = do
        b <- getWord8
        let fin = wordHasStreamFin b
            hasDataLen = wordHasStreamDataLenField b
            offsetSize = word2streamOffsetSize b
            streamIDSize = word2streamStreamIdSize b

        streamID <- getStreamID streamIDSize
        offset <- getOffset offsetSize
        dataLen <- if hasDataLen then getInt2byte else return 0
        streamData <- getData dataLen
        return $ Right $ StreamFrame fin hasDataLen offset streamID streamData

    getData :: Int -> Get BS.ByteString
    getData 0 = toStrict <$> getRemainingLazyByteString 
    getData n = getByteString $ fromIntegral n



wordHasStreamFin :: Word8 -> Bool
wordHasStreamFin i = i .&. 0x40  == 0x40

wordHasStreamDataLenField :: Word8 -> Bool
wordHasStreamDataLenField i = i .&. 0x20 == 0x20


word2streamOffsetSize :: Word8 -> ByteSize
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

streamOffsetSize2word :: Int -> Word8
streamOffsetSize2word i = case i of
                         1 -> 0x00
                         2 -> 0x01
                         4 -> 0x02
                         8 -> 0x03
                         _ -> undefined

word2streamStreamIdSize :: Word8 -> ByteSize
word2streamStreamIdSize i = case i .&. 0x03 of
                       0x00 -> 1
                       0x01 -> 2
                       0x02 -> 4
                       0x03 -> 8
                       _    -> undefined

streamStreamIDSize2word :: Int -> Word8
streamStreamIDSize2word = undefined


putData :: BS.ByteString -> Put
putData = undefined

encodeStreamFrame :: StreamFrame -> BSL.ByteString
encodeStreamFrame (StreamFrame hasFin hasDataLen offset streamID streamData) = runPut $ put 
  where
    put :: Put
    put = do
      putWord8 flag
      putStreamID  streamIDSize streamID
      putOffset offsetSize offset
      putData streamData
      where
        streamIDSize = countStreamIDbyteSize streamID
        offsetSize = countOffsetByteSize offset
        flag = 0x80 
          .|.  (if hasFin then 0x40 else 0x00)
          .|. (if hasDataLen then 0x20 else 0x00)
          .|. (streamOffsetSize2word offsetSize) 
          .|. (streamStreamIDSize2word streamIDSize)
          .|. 0x00
      
