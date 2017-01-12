module Network.QUIC.Error
  (
      ErrorCodes(..)
    , err2int
    , int2err
    , QUICResult
  )
  where

type QUICResult a = Either ErrorCodes a

data ErrorCodes = NoError
                | StreamDataAfterTermination
                | ServerErrorProcessingStream
                | MultipleTerminationOffsets
                | BadApplicationPayload
                | InvalidPacketHeader
                | InvalidFrameData
                | InvalidFecData
                | InvalidRstStreamData
                | InvalidConnectionCloseData
                | InvalidAckData
                | DecryptionFailure
                | EncryptionFailure
                | PacketTooLarge
                | PacketForNoexistentStream
                | ClientGoingAway
                | InvalidStreamId
                | TooManyOpenStreams
                | ConnectionTimeOut
                | CryptoTooManyEntries
                | CryptoInvalidValueLength
                | CryptoMessageAfterHandshakeComplete
                | InvalidCryptoMessageType
                | SequenceNumberLimit2Reached
                | Undefined
                deriving (Show, Eq)

err2int :: ErrorCodes -> Int
err2int NoError                             = 0
err2int StreamDataAfterTermination          = 1
err2int ServerErrorProcessingStream         = 2
err2int MultipleTerminationOffsets          = 3
err2int BadApplicationPayload               = 4
err2int InvalidPacketHeader                 = 5
err2int InvalidFrameData                    = 6
err2int InvalidFecData                      = 7
err2int InvalidRstStreamData                = 8
err2int InvalidConnectionCloseData          = 9
err2int InvalidAckData                      = 10
err2int DecryptionFailure                   = 11
err2int EncryptionFailure                   = 12
err2int PacketTooLarge                      = 13
err2int PacketForNoexistentStream           = 14
err2int ClientGoingAway                     = 15
err2int InvalidStreamId                     = 16
err2int TooManyOpenStreams                  = 17
err2int ConnectionTimeOut                   = 18
err2int CryptoTooManyEntries                = 19
err2int CryptoInvalidValueLength            = 20
err2int CryptoMessageAfterHandshakeComplete = 21
err2int InvalidCryptoMessageType            = 22
err2int _                                   = undefined

int2err  :: Int -> ErrorCodes
int2err _ =  Undefined
