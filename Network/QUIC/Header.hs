{-# LANGUAGE CPP #-}
module Network.QUIC.Header
  (
      Header(..)
  ) where
import           Data.Int
import Data.Binary
import Data.Binary.Get
import Data.Bits
import Data.Word 

data Header = RegularHeader { regularHeaderConnectionId         :: Maybe Int64
            ,regularHeaderQuicVersion          :: Maybe Int32
            ,regularHeaderDiversificationNonce :: Integer
            ,regularHeaderPacketNumber         :: Integer
            } 

{-

decodeHeader ::  ByteString -> QUICResult (Header, ByteString)
decodeHeader bs = case runGetOrFail get  bs of
                         Right (bs, _, h) -> Right (h, bs)
                         Left _ -> undefined
  where
    get ::  Get Header
    get  = flag >>= (check s)
      where
        check ::  Flags -> Get Header
        check  f@(Flags v nonce reset cid n path)
          | reset == True = decodePublicReset s
          | (v == True && (S.isClient $ endpoint s)) =  decodeVersionNegotiation
          | otherwise = decodePublicReset s 
        decodePublicReset ::  Settings -> Get Header
        decodePublicReset  = undefined

        decodeVersionNegotiation :: Get Header
        decodeVersionNegotiation = undefined

        decodeRegular :: Settings -> Flags -> Get Header
        decodeRegular s f = RegularHeader f <$> (conn $ flagsConnectionId f) <*> (ver $ flagsVersion f ) <*> (fromIntegral <$> getIntN 32) <*> num
-}
