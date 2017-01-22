module Network.QUIC.Packet where
import qualified Network.QUIC.Header as Header
import qualified Network.QUIC.Frame as Frame
import Data.Word

data Table

data Packet  = VersionNegotiation {  versionNegotiationFlag  :: Header.Flags
                                  ,  versionNegotiationVersion :: [Word32]

             | PublicReset { publicResetFlags :: Header.Flags
                           , publicResetConnectionId :: Word64
                           , publicResetMap :: Table }
             | Regular { regularHeader :: Header.CommonHeader 
                       , regularFrame :: Frame.Frame
                       }
