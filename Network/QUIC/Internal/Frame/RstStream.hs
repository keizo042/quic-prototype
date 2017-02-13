module Network.QUIC.Internal.Frame.RstStream 
  (
    RstStreamFrame(..)
    , encodeRstStreamFrame
    , decodeRstStreamFrame
  )where
import Data.Binary.Get
import Data.Binary.Put

import Data.ByteString.Lazy

import qualified Network.QUIC.Error as E
import Network.QUIC.Types
import Network.QUIC.Internal.Util.Binary

data RstStreamFrame = RstStreamFrame  { rstStreamStreamID   :: !StreamID 
                                      , rstStreamByteOffset :: !Offset
                                      , rstStreamErrCode    :: !E.ErrorCodes
                                      } deriving Show


encodeRstStreamFrame :: RstStreamFrame -> ByteString
encodeRstStreamFrame (RstStreamFrame streamID offset errCode) = runPut put
  where
    put :: Put
    put = putStreamID (countStreamIDbyteSize streamID) streamID >> 
          putOffset (countOffsetByteSize offset) offset >>
          putErrorCode errCode

decodeRstStreamFrame :: ByteString -> E.QUICResult (RstStreamFrame, ByteString)
decodeRstStreamFrame bs =  case runGetOrFail (get n) bs of
                                  Right (bs, _, frame) -> case frame of
                                                               Right f -> Right (f, bs)
                                                               Left e -> Left e
                                  Left _    -> Left E.InvalidRstStreamData
  where
    n = 4
    m = 8
    get :: Int -> Get (E.QUICResult RstStreamFrame)
    get n =  Right <$> 
        (RstStreamFrame <$> 
         getStreamID n <*> 
         getOffset m <*> 
         getErrorCode)
