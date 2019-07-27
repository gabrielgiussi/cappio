package oss.giussi.cappio.impl.net

import oss.giussi.cappio.{FLLDeliver, Mod, NextState, Packet}

object FairLossLink {

  case class FLLSend(packet: Packet) {
    // TODO
    def asDeliver = FLLDeliver(packet)
  }

}

trait Socket[M <: Mod] {
  def deliver(packet: FLLDeliver): NextState[M]
}