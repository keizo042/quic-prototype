module Network.QUIC.Packet where
import qualified Network.QUIC.Header as H
import Network.QUIC.Frame
import Network.QUIC.Types
import Data.Word


data Packet  = VersionNegotiation {  versionNegotiationFlag  :: H.Header
                                  ,  versionNegotiationVersion :: [Int] }
             | PublicReset { publicResetFlags :: H.Header
                           , publicResetConnectionID :: ConnectionID
                           , publicResetMap :: Table }
             | Regular { regularHeader :: H.Header 
                       , regularPayload :: Payload }
              deriving Show
