module Network.QUIC.Packet where
import qualified Network.QUIC.Header as Header
import qualified Network.QUIC.Frame as Frame
import Data.Word

data Table

data Packet  = VersionNegotiation {  versionNegotiationFlag  :: Header.Header
                                  ,  versionNegotiationVersion :: [Int]

             | PublicReset { publicResetFlags :: Header.Header
                           , publicResetConnectionId :: Int
                           , publicResetMap :: Table }
             | Regular { regularHeader :: Header.Header 
                       , regularFrame :: Frame.Frame
                       }
