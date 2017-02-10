module Network.QUIC.Internal.Util.Binary 
  (   getInt1byte
    , getInt2byte
    , getInt3byte
    , getInt4byte
    , getInt6byte
    , getInt7byte
    , getInt8byte
    , getIntNbyte

    , getStreamID
    , getErrorCode
  )where
import Data.Int(Int8)
import Data.Bits
import Data.Binary
import qualified Data.Binary.Get as Get
import qualified Network.QUIC.Error as E


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

getStreamID :: Int -> Get Int
getStreamID n =  getIntNbyte n

getErrorCode :: Get E.ErrorCodes
getErrorCode = E.int2err <$> getInt4byte
