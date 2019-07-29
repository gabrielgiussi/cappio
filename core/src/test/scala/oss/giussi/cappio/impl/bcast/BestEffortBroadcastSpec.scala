package oss.giussi.cappio.impl.bcast

import org.scalatest.{Matchers, WordSpec}
import oss.giussi.cappio.impl.bcast.BestEffortBroadcast.{BebBcast, BebDeliver}
import oss.giussi.cappio._

class BestEffortBroadcastSpec extends WordSpec with Matchers {

  implicit def intToProcessId(id: Int) = ProcessId(id)

  val instance = Instance("beb")
  val all = Set(0, 1, 2).map(ProcessId)
  val beb = BestEffortBroadcast.init[String](ProcessId(0), all, 3)

  "BestEfforBroadcast" must {
    "A" in {
      beb.request(BebBcast(Payload("something"), instance))
        .send.map(_.packet).map(p => (p.from.id, p.to.id, p.payload)) should contain theSameElementsAs Set(0, 1, 2).map(to => (0, to, "something"))
    }
    "B" in {
      val packet = Packet(0,1,"s",instance)
      beb.tail.deliver(FLLDeliver(packet))
        .indications should contain theSameElementsAs List(BebDeliver(0,Payload(packet.id,"s")))
    }
  }

}
