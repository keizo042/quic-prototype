module Network.QUIC.Internal.Util.Binary 
  (   getInt1byte
    , getInt2byte
    , getInt3byte
    , getInt4byte
    , getInt6byte
    , getInt7byte
    , getInt8byte
    , getIntNbyte

    , putIntNbyte

    , getOffset
    , putOffset

    , getStreamID
    , putStreamID

    , getConnectionID
    , putConnectionID

    , getErrorCode
    , putErrorCode

    , putReasonPhase

  )where
import Data.Int(Int8)
import Data.Bits
import Data.Binary
import qualified Data.ByteString as BS
import qualified Data.Binary.Get as Get
import qualified Network.QUIC.Error as E
import Network.QUIC.Internal 
import Network.QUIC.Types 


getInt1byte :: Get Int
getInt1byte = getIntNbyte 1

getInt2byte :: Get Int
getInt2byte = getIntNbyte 2

getInt3byte :: Get Int
getInt3byte = getIntNbyte 3

getInt4byte  :: Get Int
getInt4byte = getIntNbyte 4

getInt6byte :: Get Int
getInt6byte = getIntNbyte 6

getInt7byte :: Get Int
getInt7byte = getIntNbyte 7

getInt8byte :: Get Int
getInt8byte = getIntNbyte 8

getIntNbyte :: Int -> Get Int
getIntNbyte 0 = error "fail"
getIntNbyte n = foldl f 0 <$> list
  where
    f :: Int -> (Int8, Int) -> Int
    f n (x,i) = n + (shiftL (i * 8) $ toInt x)

    list :: Get [(Int8, Int)]
    list = (\xs -> zip xs [0..])  <$> (sequence $ replicate n Get.getInt8)  

    toInt = fromIntegral . toInteger

putIntNbyte :: Int -> [Word8] ->  Put
putIntNbyte = undefined

getStreamID :: ByteSize -> Get StreamID
getStreamID n =  getIntNbyte n

putStreamID :: ByteSize -> StreamID -> Put
putStreamID = undefined

getConnectionID :: ByteSize -> Get ConnectionID
getConnectionID = undefined

putConnectionID :: ByteSize -> Get ConnectionID
putConnectionID = undefined

getOffset :: ByteSize -> Get Offset
getOffset = undefined

putOffset :: ByteSize -> Offset -> Put
putOffset = undefined


getErrorCode :: Get E.ErrorCodes
getErrorCode = E.int2err <$> getInt4byte

putErrorCode :: E.ErrorCodes -> Put
putErrorCode e = do
  putIntNbyte 4  (convert $ E.err2int e)
  where 
        convert :: Int -> [Word8]
        convert = undefined

getReasonPhase :: Get E.ErrorCodes
getReasonPhase = undefined

putReasonPhase :: BS.ByteString -> Put
putReasonPhase = undefined

