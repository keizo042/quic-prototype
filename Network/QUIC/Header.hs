{-# LANGUAGE CPP #-}
module Network.QUIC.Header
  (
      Header(..)
    , Flags(..)
    , word82flags
    , flags2word8
  ) where
import           Data.Int
import Data.Bits
import Data.Word 

data Header = RegularHeader { 
             regularHeaderFlags                :: Flags
            ,regularHeaderConnectionId         :: Maybe Int64
            ,regularHeaderQuicVersion          :: Maybe Int32
            ,regularHeaderDiversificationNonce :: Integer
            ,regularHeaderPacketNumber         :: Integer
            } 
            | PublicResetHeader   { publicResetFlags :: Flags }
            | VersionNegotiation  { versionNegotiationFlags :: Flags }


data Flags = Flags {
    flagsVersion                  :: Bool
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

decodeHeader ::  ByteString -> QUICResult (Header, ByteString)
decodeHeader bs = case BG.runGetOrFail get  bs of
                         Right (bs, _, h) -> Right (h, bs)
                         Left _ -> undefined
  where
    get ::  BG.Get Header
    get  = flag >>= (check s)
      where
        check ::  Flags -> BG.Get Header
        check  f@(Flags v nonce reset cid n path)
          | reset == True = decodePublicReset s
          | (v == True && (S.isClient $ endpoint s)) =  decodeVersionNegotiation
          | otherwise = decodePublicReset s 
        decodePublicReset ::  Settings -> BG.Get Header
        decodePublicReset  = undefined

        decodeVersionNegotiation :: BG.Get Header
        decodeVersionNegotiation = undefined

        decodeRegular :: Settings -> Flags -> BG.Get Header
        decodeRegular s f = RegularHeader f <$> (conn $ flagsConnectionId f) <*> (ver $ flagsVersion f ) <*> (fromIntegral <$> BG.getIntN 32) <*> num

