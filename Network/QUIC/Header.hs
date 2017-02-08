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

