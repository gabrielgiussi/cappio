package oss.giussi.cappio.impl.net
import oss.giussi.cappio.impl.net.PerfectLink.{PLDeliver, PLSend, PerfectLinkState}
import oss.giussi.cappio.impl.net.StubbornLink.{SLDeliver, SLSend, StubbornLinkState}
import oss.giussi.cappio.{FLLDeliver, Module, NextState, Packet}

object PerfectLink {

  case class PLSend(packet: Packet)

  case class PLDeliver(packet: Packet)

  type PerfectLinkModule = Module[PLSend,PerfectLinkState,PLDeliver]

  case class PerfectLinkState(delivered: Set[Packet]) {
    def alreadyDelivered(d: Packet): Boolean = delivered contains d

    def deliver(p: Packet): PerfectLinkState = copy(delivered + p)
  }

  def init(timeout: Int) = PerfectLink(PerfectLinkState(Set.empty),StubbornLink.init(timeout))

}


case class PerfectLink(state: PerfectLinkState, st: Module[SLSend, StubbornLinkState, SLDeliver]) extends Module[PLSend,PerfectLinkState,PLDeliver]{

  override def request(send: PLSend): Next = {
    val NextState(_, s, nst) = st.request(SLSend(send.packet)) // No le doy bola a las indications porque para abajo no puede haber, solo sends.
    next(copy(st = nst), send = s)
  }

  override def tail = (packet: FLLDeliver) => {
    val NextState(i, _, nst) = st.tail.deliver(packet) // Yo se que el SL no puede hacer sends si esta haciendo un deliver! Esto deberia ser algo manejado de manera generica
    next(copy(st = nst), indications = Set(PLDeliver(i.head.packet))) // FIXME i.head
  }

  // aca esta el problema de que modulo tiene que encargarse de delegar al hijo el tick. Puedo hacer una clase que implemente el tick delegando, pero el problema
  // sigue estando, el tema es que si entro "por afuera" a cada abstraccion, no tengo control de las indications que puedan salir como resultado del tick.
  override def tick: Next = {
    val NextState(_,s,nst) = st.tick
    next(copy(st = nst), send = s)
  }
}