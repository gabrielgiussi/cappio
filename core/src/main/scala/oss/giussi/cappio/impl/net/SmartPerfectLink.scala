package oss.giussi.cappio.impl.net

import oss.giussi.cappio.impl.net.FairLossLink.FLLSend
import oss.giussi.cappio.impl.net.PerfectLink.{PLDeliver, PLSend}
import oss.giussi.cappio.impl.net.SmartPerfectLink.SPLMod
import oss.giussi.cappio.{FLLDeliver, Mod, Module, NextState, Packet}


object SmartPerfectLinkState {

  def init[P](timeout: Int) = SmartPerfectLinkState(Set.empty[Packet[P]], 0, timeout)

}

case class SmartPerfectLinkState[P](undelivered: Set[Packet[P]], timer: Int, timeout: Int) {
  def delivered(p: Packet[P]) = copy(undelivered = undelivered - p)

  def send(p: Packet[P]) = copy(undelivered = undelivered + p)

  def tick(): (SmartPerfectLinkState[P], Set[Packet[P]]) = {
    if (timer + 1 == timeout) (copy(timer = 0), undelivered)
    else (copy(timer = timer + 1), Set.empty)
  }
}

object SmartPerfectLink {

  def init(timeout: Int) = SmartPerfectLink(SmartPerfectLinkState.init(timeout))

  type SPLMod[P] = Mod {
    type Req = PLSend[P]
    type State = SmartPerfectLinkState[P]
    type Ind = PLDeliver[P]
    type Payload = P
  }
}

case class SmartPerfectLink[P](state: SmartPerfectLinkState[P]) extends Module[SPLMod[P]] with Socket[SPLMod[P]] {
  override def request(in: PLSend[P]): Next = {
    next(copy(state.send(in.packet)), send = Set(FLLSend(in.packet)))
  }

  override def tail: Socket[SPLMod[P]] = this

  override def tick: Next = {
    val (ns, packets) = state.tick()
    next(copy(ns), send = packets.map(FLLSend.apply))
  }

  override def deliver(d: FLLDeliver[P]): NextState[SPLMod[P]] = next(copy(state.delivered(d.packet)), indications = Set(PLDeliver(d.packet)))
}
