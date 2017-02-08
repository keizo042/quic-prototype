module Network.QUIC.Internal.Flags where
import Data.Binary.Get
import Data.Binary.Put
import Data.Int

getFlags :: Get Flags
getFlags = word82flags <$> fromIntegral <$> getInt8
  where
  conn :: Bool -> Get (Maybe Int64)
  conn True = getInt64 >>=  return . Just . fromIntegral 
  conn False = return Nothing
  ver :: Bool -> Get (Maybe Int32)
  ver True = getInt32 >>= return . Just . fromIntegral
  ver False =  return Nothing
  num :: Get Integer
  num = undefined

data Flags = Flags { flagsVersion                  :: Bool
                   , flagsPublicReset          :: Bool
                   , flagsDiversificationNonce :: Integer
                   , flagsConnectionId         :: Bool
                   , flagsPacketNumberSize     :: Int
                   , flagsMultipath            :: Int
                    } deriving (Show)

flags2word8 :: Flags -> Word8
flags2word8 (Flags v p nonce conn n mp)  =  (a v) .|. (b p) .|. (c nonce) .|. (d conn) .|. (e n) .|. (f mp)
  where
      a :: Bool -> Word8
      a True = undefined
      a False = undefined

      b :: Bool -> Word8
      b True = undefined
      b False = undefined

      c :: Bool -> Word8
      c True = undefined
      c False = undefined

      d :: Bool -> Word8
      d True = undefined
      d False = undefined

      e :: Int -> Word8
      e n = undefined

      f :: Int -> Word8
      f n = undefined

word82flags :: Word8 -> Flags
word82flags w = Flags (a w) (b w) (c w) (d w) (e w) (f w) 
  where
    a = undefined
    b = undefined
    c = undefined
    d = undefined
    e = undefined
    f = undefined
