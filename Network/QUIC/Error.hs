module Network.QUIC.Error
  (
      ErrorCodes(..)
    , err2int
    , int2err
    , QUICResult
  )
  where

type QUICResult a = Either ErrorCodes a

data ErrorCodes = InternalError
                | StreamDataAfterTermination
                | InvalidPacketError
                | InvalidFrameData
                | MissingPayload
                | InvalidStreamData
                | OverlappingStreamData
                | UnencryptedStreamData
                | AttemptToSendUnencryptedStreamData
                | MaybeCrruptedMemory
                | InvalidRstStreamData
                | InvalidConnectionCloseData
                | InvalidGoAwayData
                | InvalidWindowUpdateData
                | InvalidBlockedData
                | InvalidStopWaitingData
                | InvalidPathCloseData
                | InvalidAckData
                | InvalidVersionNegotiationPacket
                | InvalidPublicRstPacket
                | DecryptionFailure
                | EncryptionFailure
                | PacketTooLarge
                | PeerGoingAway
                | InvalidStreamId
                | InvalidPriority
                | TooManyOpenStreams
                | TooManyAvailableStreams
                | PublicReset
                | InvalidVersion
                | InvalidHeaderId
                | InvalidNegtioatedValue
                | DecompromissionFaliure
                | NetworkIdleTimeout
                | HandshakeTimeout
                | ErrorMigratingAddress
                | ErrorMigratingPort
                | PacketWritingError
                | PacketReadingError
                | EmptyStreamFrameNoFin
                | InvalidHeadersStreamData
                | FlowControlReceivedTooMuchData
                | FlowControlSentTooMuchData
                | FlowControlInvalidWindow
                | ConnectionIpPooled
                | TooManyOutstandingSentPackets
                | TooManyOutstandingRecivedPackets
                | ConnectionCancelled
                | BadPacketLossRate
                | PublicResetPortHandshake
                | TimeoutsWithOpenStreams
                | FailedToSerializePacket
                | TooManyRtos
                | CryptoTagsOutOfOrder
                | CryptoTooManyEntries
                |CryptoInvalidValueLength
                | CryptoMessageAfterHandshakeComplete
                | InvaildCryptoMessageType
                | InvaildCryptoMessageParameter
                | InvaildChannelIdSignature
                | CryptoMessagePrarameterNotFound
                | CryptoMessagePramaterNoOverlap
                | CryptoMessageIndexNotFound
                | UnsupportedProofDemand
                | CryptoInternalError
                | CryptoVersionNotSupported
                | CryptoHandshakeStatelessReject
                | CryptoNoSupport
                | CryptoTooManyRejects
                | CryptoDuplicateTag
                | CryptoEncryptionLevelIncorrect
                | CryptoServerConfigExpired
                | CryptoSymmetricKeySetupFailed
                | CryptoMessageWhileValidingClientHello
                | CryptoUpdateBeforeHandshakeComplete
                | CryptoChloTooLarge
                | VersionNegotiationMismatch
                | IpAddressChanged
                | ConnectionMigrationNoMigratableStreams
                | ConnectionMigrationTooManyChanges
                | ConnectionMigrationNoNewNetwork
                | ConnectionMigrationNonMigratableStream
                | TooManyFrameGaps
                | StreamSequencerInvalidState
                | TooManySessionsOnServer
                deriving (Show, Eq)

err2int :: ErrorCodes -> Int
err2int InternalError                          = 0x01
err2int StreamDataAfterTermination             = 0x02
err2int InvalidPacketError                     = 0x03
err2int InvalidFrameData                       = 0x04
err2int MissingPayload                         = 0x30
err2int InvalidStreamData                      = 0x2e
err2int OverlappingStreamData                  = 0x57
err2int UnencryptedStreamData                  = 0x3d
err2int AttemptToSendUnencryptedStreamData     = 0x58
err2int MaybeCrruptedMemory                    = 0x59
err2int InvalidRstStreamData                   = 0x06
err2int InvalidConnectionCloseData             = 0x07
err2int InvalidGoAwayData                      = 0x08
err2int InvalidWindowUpdateData                = 0x39
err2int InvalidBlockedData                     = 0x3a
err2int InvalidStopWaitingData                 = 0x3c
err2int InvalidPathCloseData                   = 0x4e
err2int InvalidAckData                         = 0x09
err2int InvalidVersionNegotiationPacket        = 0x0a
err2int InvalidPublicRstPacket                 = 0x0b
err2int DecryptionFailure                      = 0x0c
err2int EncryptionFailure                      = 0x0d
err2int PacketTooLarge                         = 0x0e
err2int PeerGoingAway                          = 0x10
err2int InvalidStreamId                        = 0x11
err2int InvalidPriority                        = 0x31
err2int TooManyOpenStreams                     = 0x12
err2int TooManyAvailableStreams                = 0x4c
err2int PublicReset                            = 0x13
err2int InvalidVersion                         = 0x14
err2int InvalidHeaderId                        = 0x16
err2int InvalidNegtioatedValue                 = 0x17
err2int DecompromissionFaliure                 = 0x18
err2int NetworkIdleTimeout                     = 0x19
err2int HandshakeTimeout                       = 0x43
err2int ErrorMigratingAddress                  = 0x1a
err2int ErrorMigratingPort                     = 0x56
err2int PacketWritingError                     = 0x1b
err2int PacketReadingError                     = 0x33
err2int EmptyStreamFrameNoFin                  = 0x32
err2int InvalidHeadersStreamData               = 0x38
err2int FlowControlReceivedTooMuchData         = 0x3b
err2int FlowControlSentTooMuchData             = 0x3f
err2int FlowControlInvalidWindow               = 0x40
err2int ConnectionIpPooled                     = 0x3e
err2int TooManyOutstandingSentPackets          = 0x44
err2int TooManyOutstandingRecivedPackets       = 0x45
err2int ConnectionCancelled                    = 0x46
err2int BadPacketLossRate                      = 0x47
err2int PublicResetPortHandshake               = 0x49
err2int TimeoutsWithOpenStreams                = 0x4a
err2int FailedToSerializePacket                = 0x4b
err2int TooManyRtos                            = 0x55
err2int CryptoTagsOutOfOrder                   = 0x1d
err2int CryptoTooManyEntries                   = 0x1e
err2int CryptoInvalidValueLength               = 0x1f
err2int CryptoMessageAfterHandshakeComplete    = 0x20
err2int InvaildCryptoMessageType               = 0x21
err2int InvaildCryptoMessageParameter          = 0x22
err2int InvaildChannelIdSignature              = 0x34
err2int CryptoMessagePrarameterNotFound        = 0x23
err2int CryptoMessagePramaterNoOverlap         = 0x24
err2int CryptoMessageIndexNotFound             = 0x25
err2int UnsupportedProofDemand                 = 0x5e
err2int CryptoInternalError                    = 0x26
err2int CryptoVersionNotSupported              = 0x27
err2int CryptoHandshakeStatelessReject         = 0x48
err2int CryptoNoSupport                        = 0x28
err2int CryptoTooManyRejects                   = 0x29
err2int CryptoDuplicateTag                     = 0x2b
err2int CryptoEncryptionLevelIncorrect         = 0x2c
err2int CryptoServerConfigExpired              = 0x2d
err2int CryptoSymmetricKeySetupFailed          = 0x35
err2int CryptoMessageWhileValidingClientHello  = 0x36
err2int CryptoUpdateBeforeHandshakeComplete    = 0x41
err2int CryptoChloTooLarge                     = 0x5a
err2int VersionNegotiationMismatch             = 0x37
err2int IpAddressChanged                       = 0x50
err2int ConnectionMigrationNoMigratableStreams = 0x51
err2int ConnectionMigrationTooManyChanges      = 0x52
err2int ConnectionMigrationNoNewNetwork        = 0x53
err2int ConnectionMigrationNonMigratableStream = 0x54
err2int TooManyFrameGaps                       = 0x5d
err2int StreamSequencerInvalidState            = 0x5f
err2int TooManySessionsOnServer                = 0x60

int2err  :: Int -> ErrorCodes
int2err 0x01 = InternalError
int2err 0x02 = StreamDataAfterTermination
int2err 0x03 = InvalidPacketError             
int2err 0x04 = InvalidFrameData   
int2err 0x30 = MissingPayload      
int2err 0x2e = InvalidStreamData    
int2err 0x57 = OverlappingStreamData
int2err 0x3d = UnencryptedStreamData
int2err 0x58 = AttemptToSendUnencryptedStreamData   
int2err 0x59 = MaybeCrruptedMemory                   
int2err 0x06 = InvalidRstStreamData                   
int2err 0x07 = InvalidConnectionCloseData
int2err 0x08 = InvalidGoAwayData                      
int2err 0x39 = InvalidWindowUpdateData               
int2err 0x3a = InvalidBlockedData                   
int2err 0x3c = InvalidStopWaitingData              
int2err 0x4e = InvalidPathCloseData               
int2err 0x09 = InvalidAckData                         
int2err 0x0a = InvalidVersionNegotiationPacket       
int2err 0x0b = InvalidPublicRstPacket               
int2err 0x0c = DecryptionFailure                   
int2err 0x0d = EncryptionFailure                  
int2err 0x0e = PacketTooLarge                    
int2err 0x10 = PeerGoingAway                    
int2err 0x11 = InvalidStreamId                 
int2err 0x31 = InvalidPriority                
int2err 0x12 = TooManyOpenStreams                     
int2err 0x4c = TooManyAvailableStreams               
int2err 0x13 = PublicReset                          
int2err 0x14 = InvalidVersion                      
int2err 0x16 = InvalidHeaderId                    
int2err 0x17 = InvalidNegtioatedValue            
int2err 0x18 = DecompromissionFaliure                 
int2err 0x19 = NetworkIdleTimeout                    
int2err 0x43 = HandshakeTimeout                     
int2err 0x1a = ErrorMigratingAddress               
int2err 0x56 = ErrorMigratingPort                 
int2err 0x1b = PacketWritingError                
int2err 0x33 = PacketReadingError               
int2err 0x32 = EmptyStreamFrameNoFin                  
int2err 0x38 = InvalidHeadersStreamData              
int2err 0x3b = FlowControlReceivedTooMuchData       
int2err 0x3f = FlowControlSentTooMuchData          
int2err 0x40 = FlowControlInvalidWindow           
int2err 0x3e = ConnectionIpPooled                
int2err 0x44 = TooManyOutstandingSentPackets    
int2err 0x45 = TooManyOutstandingRecivedPackets
int2err 0x46 = ConnectionCancelled                    
int2err 0x47 = BadPacketLossRate                     
int2err 0x49 = PublicResetPortHandshake             
int2err 0x4a = TimeoutsWithOpenStreams             
int2err 0x4b = FailedToSerializePacket            
int2err 0x55 = TooManyRtos                       
int2err 0x1d = CryptoTagsOutOfOrder             
int2err 0x1e = CryptoTooManyEntries            
int2err 0x1f = CryptoInvalidValueLength       
int2err 0x20 = CryptoMessageAfterHandshakeComplete    
int2err 0x21 = InvaildCryptoMessageType              
int2err 0x22 = InvaildCryptoMessageParameter        
int2err 0x34 = InvaildChannelIdSignature           
int2err 0x23 = CryptoMessagePrarameterNotFound    
int2err 0x24 = CryptoMessagePramaterNoOverlap    
int2err 0x25 = CryptoMessageIndexNotFound       
int2err 0x5e = UnsupportedProofDemand          
int2err 0x26 = CryptoInternalError            
int2err 0x27 = CryptoVersionNotSupported     
int2err 0x48 = CryptoHandshakeStatelessReject 
int2err 0x28 = CryptoNoSupport                 
int2err 0x29 = CryptoTooManyRejects             
int2err 0x2b = CryptoDuplicateTag                
int2err 0x2c = CryptoEncryptionLevelIncorrect     
int2err 0x2d = CryptoServerConfigExpired           
int2err 0x35 = CryptoSymmetricKeySetupFailed        
int2err 0x36 = CryptoMessageWhileValidingClientHello 
int2err 0x41 = CryptoUpdateBeforeHandshakeComplete    
int2err 0x5a = CryptoChloTooLarge                 
int2err 0x37 = VersionNegotiationMismatch          
int2err 0x50 = IpAddressChanged                     
int2err 0x51 = ConnectionMigrationNoMigratableStreams
int2err 0x52 = ConnectionMigrationTooManyChanges   
int2err 0x53 = ConnectionMigrationNoNewNetwork      
int2err 0x54 = ConnectionMigrationNonMigratableStream
int2err 0x5d = TooManyFrameGaps                       
int2err 0x5f = StreamSequencerInvalidState           
int2err 0x60 = TooManySessionsOnServer                
int2err _ = error "undefined erorr number"
