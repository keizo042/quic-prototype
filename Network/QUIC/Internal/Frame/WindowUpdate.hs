module Network.QUIC.Internal.Frame.WindowUpdate where
import Data.Binary.Get
import Data.Binary.Put

import Network.QUIC.Error

data WindowUpdateFrame = WindowUpdateFrame { windowUpdateStreamId   :: Int
                                           , windowUpdateByteOffset :: Int
                                           } deriving Show

encodeWindowUpdateFrame :: WindowUpdateFrame -> ByteString
encodeWindowUpdateFrame = runPut put
  where
    put (WindowUpdateFrame i offset) = undefined

decodeWindowUpdateFrame ::  ByteString -> QUICResult (WindowUpdateFrame, ByteString)
decodeWindowUpdateFrame bs = case runGetOrFail get bs of
                               Right (bs, _, frame) -> Right (frame, bs)
                               Left _               -> Left Error.InvalidWindowUpdateData
  where
    get :: Get WindowUpdateFrame
    get  = WindowUpdateFrame <$> getInt32 <*> getInt64


