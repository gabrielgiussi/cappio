package oss.giussi.cappio.impl.net

import oss.giussi.cappio.impl.net.FairLossLink.FLLSend
import oss.giussi.cappio.impl.net.StubbornLink.{SLDeliver, SLSend, StubbornLinkState}
import oss.giussi.cappio.{Module, _}

object StubbornLink {

  // instance? es algo propio de todas las actions asi q podria ser una class Action[T](instance: Instance, action: T)
  case class SLSend(packet: Packet)

  case class SLDeliver(packet: Packet)

  case class StubbornLinkState(sent: Set[Packet], timer: Int, timeout: Int) {
    def add(s: Packet) = copy(sent = sent + s)

    def tick() = copy(timer = timer + 1)

    def reset() = copy(timer = 0)
  }

  def init(timeout: Int): StubbornLink = StubbornLink(StubbornLinkState(Set.empty, 0, timeout))
}

case class StubbornLink(state: StubbornLinkState) extends Module[SLSend, StubbornLinkState, SLDeliver] with Socket[SLSend,StubbornLinkState,SLDeliver] {

  override def request(send: SLSend) = next(copy(state.add(send.packet)),send = Set(FLLSend(send.packet)))

  override def tail: Socket[SLSend, StubbornLinkState, SLDeliver] = this

  // TODO deberia tener el ProcessId aca asi puedo validar q el deliver sea correcto!
  override def deliver(d: FLLDeliver): NextState[SLSend, StubbornLinkState, SLDeliver] = next( this, indications = Set(SLDeliver(d.packet)))

  override def tick: Next = {
    if (state.timer + 1 == state.timeout) {
      val send = state.sent.map(FLLSend.apply)
      next(copy(state.reset),send = send)
    }
    else
      next(copy(state.tick))
  }
}
