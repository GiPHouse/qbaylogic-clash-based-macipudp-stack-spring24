{-# language BlockArguments #-}
{-# language FlexibleContexts #-}
{-# language RecordWildCards #-}

{-|
Module      : Clash.Cores.Ethernet.Arp
Description : Top level ARP module.
-}
module Clash.Cores.Ethernet.Arp
  ( arpTransmitter
  , arpReceiverC
  ) where

import Clash.Prelude

import Protocols
import Protocols.Df qualified as Df
import Protocols.Df.Extra ( filterS, partitionS )
import Protocols.Extra.PacketStream
import Protocols.Extra.PacketStream.Packetizers ( depacketizeToDfC, packetizeFromDfC )

import Clash.Cores.Ethernet.Arp.ArpTypes
import Clash.Cores.Ethernet.IP.IPv4Types
import Clash.Cores.Ethernet.Mac.EthernetTypes


-- | Transmits ARP packets upon request.
arpTransmitter
  :: forall (dom :: Domain)
            (dataWidth :: Nat)
   . HiddenClockResetEnable dom
  => 1 <= dataWidth
  => KnownNat dataWidth
  => Signal dom MacAddress
  -- ^ Our MAC address
  -> Signal dom IPv4Address
  -- ^ Our IPv4 address
  -> Circuit (Df dom ArpLite) (PacketStream dom dataWidth EthernetHeader)
arpTransmitter ourMacS ourIPv4S = fromSignals bundleWithSrc |> packetizeFromDfC toEthernetHdr constructArpPkt
  where
    bundleWithSrc (fwdIn, bwdIn) = (bwdIn, go <$> bundle (ourMacS, ourIPv4S, fwdIn))
    go (ourMac, ourIPv4, maybeArpLite) = maybeArpLite >>= \arpLite -> Df.Data (ourMac, ourIPv4, arpLite)

    toEthernetHdr (ourMac, _, arpLite)
      = EthernetHeader {
        _macDst = _targetMac arpLite,
        _macSrc = ourMac,
        _etherType = arpEtherType
      }

    constructArpPkt (ourMac, ourIPv4, arpLite)
      = newArpPacket ourMac ourIPv4 (_targetMac arpLite) (_targetIPv4 arpLite) (_isRequest arpLite)

-- | arpReceiverC takes the incoming PacketStream
--   with an ethernet header in the meta data and
--   creates an ARP entry or an ARP response.
--   - It outputs ARP entries for ARP responses (OPER == 2)
--     and GARP messages in the form of an ARP request (OPER == 1) with
--     TPA == SPA.
--   - It outputs ARP lite responses for any other ARP request (OPER == 1 and
--     TPA /= SPA).
arpReceiverC
  :: forall (dom :: Domain) (dataWidth :: Nat)
   . HiddenClockResetEnable dom
  => KnownNat dataWidth
  => 1 <= dataWidth
  => Signal dom IPv4Address
  -> Circuit (PacketStream dom dataWidth EthernetHeader) (Df dom ArpEntry, Df dom ArpLite)
arpReceiverC myIP = circuit $ \ethStream -> do
  -- TODO:
  -- when backpressure is asserted on `arpTransmitter`,
  -- the entire arp stack will stall and this will lead
  -- to corruption on the `arpReceiver` side.
  -- This only happens when the outlink is saturated, but
  -- in the future we want to handle this.
  -- Solution: putting abortOnBackpressure (Packetbuffer) to
  -- before `depacketizetoDfC` should work, as depacketizeToDfC already
  -- implements dropping of
  arpDf <- depacketizeToDfC const -< ethStream
  arpDf' <- filterS (validArp <$> myIP) -< arpDf
  (arpRequests, arpEntries) <- partitionS (isRequest <$> myIP) -< arpDf'
  lites <- Df.map (\p -> ArpLite (_sha p) (_spa p) False) -< arpRequests
  entries <- Df.map (\p -> ArpEntry (_sha p) (_spa p)) -< arpEntries
  idC -< (entries, lites)
  where
    validArp ip ArpPacket{..} =
            _htype == 1
          && _ptype == 0x0800
          && _hlen  == 6
          && _plen  == 4
          &&(_oper == 1 && (_tpa == ip || _tpa == _spa) || _oper == 2)

    isRequest ip ArpPacket{..} = _oper == 1 && _tpa == ip
