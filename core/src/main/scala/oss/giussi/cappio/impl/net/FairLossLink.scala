package oss.giussi.cappio.impl.net

import oss.giussi.cappio.{FLLDeliver, Mod, NextState, Packet}

object FairLossLink {

  case class FLLSend[P](packet: Packet[P]) {
    // TODO
    def asDeliver = FLLDeliver(packet)
  }

}

trait Socket[M <: Mod] {
  def deliver(packet: FLLDeliver[M#Payload]): NextState[M]
}