module Clash.Cores.Ethernet.Frame (sendFrameOnPulse, testFrame) where

import Clash.Prelude

import Data.Maybe (isNothing)

type Byte = BitVector 8

-- | ARP packet captured using Wireshark
--
-- To get the CRC as well while capturing use `ethtool -K IFNAME rx-fcs on`.
-- To receive even packets with a wrong CRC use `ethtool -K IFNAME rx-all on`.
testFrame :: Vec (8+64) Byte
testFrame = preamble ++ (hexToVec 0xffffffffffff54833a354a500806000108000604000154833a354a50c0a80101ffffffffffffc0a801c500000000000000000000000000000000000026544e8b)

hexToVec :: KnownNat n => Unsigned (8*n) -> Vec n Byte
hexToVec x = unpack $ pack x

preamble :: Vec 8 Byte
preamble = replicate d7 0x55 :< sfd
    where
        sfd = 0xd5

crcCalculate :: Vec n Byte -> BitVector 32
crcCalculate _ = 0xBAADF00D

-- | Creates a static ethernet frame including preamble and CRC
--
-- NOTE: Needs to have a valid size, add padding to your payload if the size is too small.
-- NOTE:Does not include the inter-frame gap (IFG/IPG)
createStaticFrame :: forall n . (KnownNat n, (72 <=? 8+6+6+2+n+4) ~ 'True, (8+6+6+2+n+4 <=? 1530) ~ 'True)
    => Vec 6 Byte
    -> Vec 6 Byte
    -> BitVector 16
    -> Vec n Byte
    -> Vec (8 + 6 + 6 + 2 + n + 4) Byte
createStaticFrame src dest eth_type payload = preamble ++ dest ++ src ++ (unpack eth_type) ++ payload ++ crc
    where
        crc :: Vec 4 Byte
        crc = unpack $ crcCalculate $ dest ++ src ++ (unpack eth_type) ++ payload

sendFrameOnPulse :: forall dom . HiddenClockResetEnable dom
    => Signal dom Bool
    -> Signal dom (Maybe Byte)
sendFrameOnPulse inp = liftA2 output counter mem
    where
        frame_length = 72
        output c m
            | c == 0 = Nothing
            | c <= frame_length = Just m
            | c <= frame_length+12 = Nothing
            | otherwise = Nothing

        counter :: Signal dom (Unsigned 32)
        counter = register 0 (inc <$> bundle (counter, inp))
            where
                inc (i,pulse)
                    -- | i == 0 = 1
                    | i == 0 && not pulse = 0
                    | i == 0 && pulse = 1
                    | i < frame_length+12 = i+1
                    | otherwise = 0

        initialState :: Vec 72 (BitVector 8)
        -- initialState = testFrame
        initialState = createStaticFrame srcMAC broadcastMAC type_ipv4 payload
            where
                -- destMAC = replicate d6 0b1
                broadcastMAC = replicate d6 0xff
                srcMAC = replicate d6 0x03
                type_ipv4 = 0x0800
                payload = 0x42 :> replicate d45 0x00

        mem = blockRam initialState readAddr write
        readAddr :: Signal dom (BitVector 32)
        readAddr = pack <$> counter
        write = pure Nothing
