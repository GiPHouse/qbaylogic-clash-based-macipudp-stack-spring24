{-# language FlexibleContexts #-}


module Clash.Cores.Ethernet.PacketBuffer
    ( packetBuffer
    ) where

import Clash.Cores.Ethernet.PacketStream
import Clash.Prelude
import Protocols
import Protocols.Internal

import Protocols.Df qualified as Df
import Protocols.DfConv ( DfConv(fromDfCircuit), fifo )

data PacketBufferState = Waiting | Transmitting
    deriving (Generic, NFDataX)

packetBuffer
    :: forall (dataWidth :: Nat) (dom :: Domain) (metaType :: Type) (depth :: Nat) .
    HiddenClockResetEnable dom
    => KnownNat dataWidth
    => KnownNat depth
    => SNat depth
    -> (Fwd (PacketStream dom dataWidth metaType), Bwd (PacketStream dom dataWidth metaType))
    -> (Bwd (PacketStream dom dataWidth metaType), Fwd(PacketStream dom dataWidth metaType))


packetBuffer depth = undefined


insidePacketBuffer 
    :: forall (dataWidth :: Nat) (dom :: Domain) (metaType :: Type) (depth :: Nat) .
    HiddenClockResetEnable dom
    => KnownNat dataWidth
    => (Fwd (PacketStream dom dataWidth metaType), Bwd (PacketStream dom dataWidth metaType))
    -> (Fwd (PacketStream dom dataWidth metaType), Bwd (PacketStream dom dataWidth metaType))
    -> ((Bwd (PacketStream dom dataWidth metaType), Fwd(PacketStream dom dataWidth metaType)),
        (Bwd (PacketStream dom dataWidth metaType), Fwd(PacketStream dom dataWidth metaType)))

insidePacketBuffer packetBuffer fifo = (unbundle resPacketBuffer, unbundle resFifo)
    where 
    (resPacketBuffer, resFifo) = unbundle $ mealy go s0 (bundle (bundle packetBuffer, bundle fifo))
        where
            s0 = Waiting
            go :: PacketBufferState
                -> ((Maybe (PacketStreamM2S dataWidth metaType), PacketStreamS2M), -- input from the packet buffer, input backpressure from the packet buffer 
                    (Maybe (PacketStreamM2S dataWidth metaType), PacketStreamS2M)) -- input from fifo, input backpressure from fifo
                -> (PacketBufferState, 
                        ((PacketStreamS2M, Maybe (PacketStreamM2S dataWidth metaType)), -- output backpressure to packet buffer, output to the packet buffer
                        (PacketStreamS2M, Maybe (PacketStreamM2S dataWidth metaType)))  -- output backpressure to fifo, output to the fifo 
                    )
            -- go Waiting (Nothing, packetBackpressure) (Nothing, fifoBackpressure) = (Waiting, ((packetBackpressure, Nothing), (fifoBackpressure, Nothing)))
            go Waiting ((inputPacket@(Just (PacketStreamM2S {_last = Just _})), _),(_, backFifo)) 
                = (Transmitting, ((backFifo, Nothing), (PacketStreamS2M False, inputPacket)))
            go Waiting ((inputPacket@(Just (PacketStreamM2S {_last = Nothing})), _), (_, backFifo))  
                = (Waiting, ((backFifo, Nothing), (PacketStreamS2M False, inputPacket)))
            go Waiting ((inputPacket@Nothing, _), (_, backFifo))  
                = (Waiting, ((backFifo, Nothing), (PacketStreamS2M False, inputPacket)))

            go Transmitting ((inputPacket, backPacket), (inputFifo@(Just (PacketStreamM2S {_last = Nothing})), backFifo))  
                = (Transmitting, ((backFifo, inputFifo), (backPacket, inputPacket)))
            go Transmitting ((inputPacket, PacketStreamS2M False), (inputFifo@(Just (PacketStreamM2S {_last = Just _})), backFifo))  
                = (Transmitting, ((backFifo, inputFifo), (PacketStreamS2M False, inputPacket)))
            go Transmitting ((inputPacket, PacketStreamS2M True), (inputFifo@(Just (PacketStreamM2S {_last = Just _})), backFifo))     
                = (Waiting, ((backFifo, inputFifo), (PacketStreamS2M False, inputPacket)))
            
            -- go Transmitting ((inputPacket, _), (Nothing, backFifo))     -- Something went wrong in the FIFO if this matches, abort
            --     = (Waiting, ((backFifo, Just (PacketStreamM2S undefined Nothing () True)), (, inputPacket)))
            -- go Transmitting ((inputPacket, backPacket), (inputFifo, backFifo))  
            --     = ( , (( , ), ( , )))
            -- TODO: We have not considered the case where the packet is larger than the FIFO, how should we deal with this?
            -- TODO: We assume the FIFO does not send Nothings
