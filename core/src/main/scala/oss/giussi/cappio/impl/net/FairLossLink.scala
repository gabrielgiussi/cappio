package oss.giussi.cappio.impl.net

import oss.giussi.cappio.{Module, NextState, Packet}
import oss.giussi.cappio.impl.net.FairLossLink.{FLLDeliver}

object FairLossLink {

  case class FLLSend(packet: Packet) {
    // TODO
    def asDeliver = FLLDeliver(packet)
  }

  // Lo correcto seria que solo el objeto network pueda instanciarlas!
  case class FLLDeliver(packet: Packet)

}

trait Socket[Req,State,In] {
  def deliver(packet: FLLDeliver): NextState[Req,State,In]
}