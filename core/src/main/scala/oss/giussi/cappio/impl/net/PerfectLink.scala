package oss.giussi.cappio.impl.net
import oss.giussi.cappio.Packet

object PerfectLink {

  case class PLSend(packet: Packet)

  case class PLDeliver(packet: Packet)


  case class PerfectLinkState(delivered: Set[Packet]) {
    def alreadyDelivered(d: Packet): Boolean = delivered contains d

    def deliver(p: Packet): PerfectLinkState = copy(delivered + p)
  }


}
