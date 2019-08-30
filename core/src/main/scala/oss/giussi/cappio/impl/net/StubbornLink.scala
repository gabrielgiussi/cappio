package oss.giussi.cappio.impl.net

import oss.giussi.cappio.impl.net.FairLossLink.FLLSend
import oss.giussi.cappio.impl.net.StubbornLink.{SLDeliver, SLSend, StubbornLinkState}
import oss.giussi.cappio.{Module, _}

object StubbornLink {

  // instance? es algo propio de todas las actions asi q podria ser una class Action[T](instance: Instance, action: T)
  case class SLSend[P](packet: Packet[P])

  case class SLDeliver[P](packet: Packet[P])

  case class StubbornLinkState[P](sent: Set[Packet[P]], timer: Int, timeout: Int) {
    def add(s: Packet[P]) = copy(sent = sent + s)

    def tick() = copy(timer = timer + 1)

    def reset() = copy(timer = 0)
  }

  def init[P](timeout: Int): StubbornLink[P] = StubbornLink(StubbornLinkState(Set.empty, 0, timeout))
}

trait StubLink[P] extends Mod {
  override type Payload = P
  override type Req = SLSend[Payload]
  override type State = StubbornLinkState[Payload]
  override type Ind = SLDeliver[Payload]
}

// TODO make a smarter StubbornLink that holds ticks for each packet and resend the packet when ticks is zero por that specifc package (and reset the counter)
case class StubbornLink[P](state: StubbornLinkState[P]) extends Module[StubLink[P]] with Socket[StubLink[P]] {

  override def request(send: SLSend[P]) = next(copy(state.add(send.packet)),send = Set(FLLSend(send.packet)))

  override def tail = this

  override def tick: Next = {
    if (state.timer + 1 == state.timeout) {
      val send = state.sent.map(FLLSend.apply)
      next(copy(state.reset),send = send)
    }
    else
      next(copy(state.tick))
  }

  override def deliver(d: FLLDeliver[P]): NextState[StubLink[P]] = next( this, indications = Set(SLDeliver(d.packet)))
}
