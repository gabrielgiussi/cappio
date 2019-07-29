package oss.giussi.cappio.impl.bcast

import oss.giussi.cappio.impl.bcast.BestEffortBroadcast.{BebBcast, BebDeliver}
import oss.giussi.cappio._

class BestEffortBroadcastSpec extends CappIOSpec {

  val instance = Instance("beb")
  val all = Set(0, 1, 2).map(ProcessId)
  val self = all.head
  val beb = BestEffortBroadcast[String](self, all,3)

  "BestEfforBroadcast" must {
    "Send the payload to all processes (including itsself)" in {
      beb.request(BebBcast(Payload("something"), instance))
        .packets.map(p => (p.from, p.to, p.payload)) should contain theSameElementsAs all.map(to => (self, to, "something"))
    }
    "Trigger a BebDeliver when packet is delivered" in {
      val packet = Packet(0,1,"s",instance)
      beb.tail.deliver(FLLDeliver(packet))
        .indications should contain theSameElementsAs List(BebDeliver(0.id,Payload(packet.id,"s")))
    }
  }

}
