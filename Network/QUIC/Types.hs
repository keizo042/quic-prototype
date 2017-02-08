module Network.QUIC.Types (
  ByteSize
  , ByteSizeInfo(..)
  , ConnectionID
  , StreamID
  , countStreamIDbyteSize
  , PacketNumber
  , Offset
  , countOffsetByteSize
  )where



type PacketNumber = Integer

type ConnectionID = Integer

-- Stream Identify
type StreamID = Int

countStreamIDbyteSize :: StreamID -> ByteSize
countStreamIDbyteSize i = undefined


-- Stream Offset 
type Offset = Integer

countOffsetByteSize :: Offset -> ByteSize
countOffsetByteSize = undefined

type ByteSize = Int

data ByteSizeInfo = NoInfo
                  | StopWaitingInfo  ByteSize
                  deriving Show
