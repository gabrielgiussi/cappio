package oss.giussi.cappio.impl.net

import oss.giussi.cappio.{FLLDeliver, Module, NextState, Packet}

object FairLossLink {

  case class FLLSend(packet: Packet) {
    // TODO
    def asDeliver = FLLDeliver(packet)
  }

}

trait Socket[Req,State,In] {
  def deliver(packet: FLLDeliver): NextState[Req,State,In]
}