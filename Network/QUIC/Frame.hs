module Network.QUIC.Frame  where
import           Data.Word
import           Network.QUIC.Error


i2f :: Word8 -> Int
i2f 0x00 = 0
i2f 0x01 = 1
i2f 0x02 = 2
i2f 0x03 = 3
i2f 0x04 = 4
i2f 0x05 = 5
i2f 0x06 = 6
i2f 0x07 = 7
i2f _    = -1

