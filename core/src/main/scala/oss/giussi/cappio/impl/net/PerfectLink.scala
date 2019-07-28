package oss.giussi.cappio.impl.net
import oss.giussi.cappio.Packet

object PerfectLink {

  case class PLSend[P](packet: Packet[P])

  case class PLDeliver[P](packet: Packet[P])


  case class PerfectLinkState[P](delivered: Set[Packet[P]]) {
    def alreadyDelivered(d: Packet[P]): Boolean = delivered contains d

    def deliver(p: Packet[P]) = copy(delivered + p)
  }


}
