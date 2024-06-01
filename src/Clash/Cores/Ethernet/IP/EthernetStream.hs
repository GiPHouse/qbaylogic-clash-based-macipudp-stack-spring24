{-# language FlexibleContexts #-}
{-# language RecordWildCards #-}
module Clash.Cores.Ethernet.IP.EthernetStream
  (toEthernetStreamC) where

import Clash.Cores.Ethernet.Arp.ArpTypes
import Clash.Cores.Ethernet.IP.IPv4Types
import Clash.Cores.Ethernet.Mac.EthernetTypes
import Clash.Prelude
import Data.Bifunctor
import Data.Maybe ( isJust )
import Protocols
import Protocols.Extra.PacketStream

-- | State of Ethernet stream transformer `toEthernetStream`
data EthernetStreamState
  = Idle
  | Drop
  | Forward {_mac :: MacAddress}
  deriving (Generic, NFDataX, Show, ShowX)

-- | Takes our IPv4 address (as a signal), a packet stream with IPv4 addresses in the metadata,
-- performs an ARP lookup from a user-given ARP service, and
-- outputs the packet with a completed ethernet header containing
-- the IPv4 ether type, our IPv4 address and the looked up destination MAC.
-- If the ARP service gave an ArpEntryNotFound, then this circuit drops the
-- entire packet. It does not time out, instead expects the ARP service to send
-- an ArpEntryNotFound after an appropriate timeout.
toEthernetStreamC
  :: forall (dom :: Domain) (dataWidth :: Nat)
  .  HiddenClockResetEnable dom
  => Signal dom MacAddress
  -- ^ My Ip address
  -> Circuit
      (PacketStream dom dataWidth IPv4Address)
      (PacketStream dom dataWidth EthernetHeader, ArpLookup dom)
toEthernetStreamC myMac = fromSignals ckt
  where
    ckt
      :: (Signal dom (Maybe (PacketStreamM2S dataWidth IPv4Address))
         , (Signal dom PacketStreamS2M, Signal dom  (Maybe ArpResponse)))
      -> (Signal dom PacketStreamS2M
         , (Signal dom (Maybe (PacketStreamM2S dataWidth EthernetHeader)),Signal dom (Maybe IPv4Address)))
    ckt (packetInS, (ackInS, arpInS)) = (second unbundle . mealyB go Idle . second bundle) (myMac, packetInS, (ackInS, arpInS))
      where
        go
          :: EthernetStreamState
          -> (MacAddress, Maybe (PacketStreamM2S dataWidth IPv4Address)
             , (PacketStreamS2M, Maybe ArpResponse))
          -> (EthernetStreamState, (PacketStreamS2M
             , (Maybe (PacketStreamM2S dataWidth EthernetHeader), Maybe IPv4Address)))
        go Idle (_, pktIn, (_, arpResponse))
          = case arpResponse of
              Nothing -> (Idle, (PacketStreamS2M False, (Nothing, fmap _meta pktIn)))
              Just ArpEntryNotFound -> (Drop, (PacketStreamS2M False, (Nothing, Nothing)))
              Just (ArpEntryFound ma) -> (Forward{_mac = ma}, (PacketStreamS2M False, (Nothing, Nothing)))
        go Drop (_, pktIn, (_, _))
          = (nextSt, (PacketStreamS2M True, (Nothing, Nothing)))
          where
            pktInX = fromJustX pktIn
            nextSt =
              if isJust pktIn && isJust (_last pktInX)
              then Idle
              else Drop
        go st@Forward{..} (mac, pktIn, (PacketStreamS2M ack, _))
          = (nextSt, (PacketStreamS2M ack, (pktOut, Nothing)))
          where
            pktInX = fromJustX pktIn
            nextSt =
              if isJust pktIn && isJust (_last pktInX) && ack
              then Idle
              else st
            hdr = EthernetHeader _mac mac 0x0800
            pktOut = fmap (hdr <$) pktIn
