package oss.giussi.cappio.impl.bcast

import org.scalatest.{Matchers, WordSpec}
import oss.giussi.cappio.impl.net.FairLossLink.FLLSend
import oss.giussi.cappio.{FLLDeliver, Packet, ProcessId}

class UniformReliableBroadcastSpec extends WordSpec with Matchers {

  val all = (0 to 2).map(ProcessId).toSet
  val timeout = 3
  val urb = UniformReliableBroadcast.init(ProcessId(0),all,timeout)
  import UniformReliableBroadcast._

  implicit def intToProcessId(i: Int): ProcessId = ProcessId(i)

  "A" should  {
    "B" in {
      val payload = Payload("gaga")
      val uuid = payload.id
      urb.request(URBBcast(payload))
        .send.map { case FLLSend(Packet(`uuid`,URBData(ProcessId(0),"gaga"),ProcessId(0),to,_)) => (to.id) } should contain theSameElementsAs Set(0,1,2)
    }
    "C" in {
      val payload = Payload("gaga")
      val urbPayload = Payload(payload.id,URBData(0,payload.msg))
      urb.request(URBBcast(payload))
        .module.tail.deliver(FLLDeliver(Packet(urbPayload,1,0,BEB)))
        .module.tail.deliver(FLLDeliver(Packet(urbPayload,2,0,BEB)))
        .module.tail.deliver(FLLDeliver(Packet(urbPayload,0,0,BEB)))
        .module.tick
        .indications should contain theSameElementsAs List(URBDeliver(0,payload.msg))
    }
  }

}
