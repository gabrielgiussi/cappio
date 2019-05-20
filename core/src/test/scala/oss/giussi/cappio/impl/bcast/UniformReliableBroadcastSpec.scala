package oss.giussi.cappio.impl.bcast

import org.scalatest.{Matchers, WordSpec}
import oss.giussi.cappio.{Packet, ProcessId}
import oss.giussi.cappio.impl.bcast.UniformReliableBroadcast.{Payload, URBBcast, URBDeliver}
import oss.giussi.cappio.impl.net.FairLossLink.{FLLDeliver, FLLSend}

class UniformReliableBroadcastSpec extends WordSpec with Matchers {

  val all = (0 to 2).map(ProcessId).toSet
  val timeout = 3
  val urb = UniformReliableBroadcast.init(ProcessId(0),all,timeout)
  import UniformReliableBroadcast._

  implicit def intToProcessId(i: Int): ProcessId = ProcessId(i)

  "A" should  {
    "B" in {
      urb.request(URBBcast(Payload("gaga")))
        .send.map { case FLLSend(Packet(_,p,from,to,_)) => (p,from.id,to.id) } should contain theSameElementsAs (0 to 2).map(("gaga",0,_))
    }
    "C" in {
      val payload = Payload("gaga")
      val urbPayload = Payload(payload.id,URBDeliverMsg(0,payload.msg))
      urb.request(URBBcast(payload))
        .module.tail.deliver(FLLDeliver(Packet(urbPayload,1,0,BEB)))
        .module.tail.deliver(FLLDeliver(Packet(urbPayload,2,0,BEB)))
        .module.tail.deliver(FLLDeliver(Packet(urbPayload,0,0,BEB)))
        .module.tick
        .indications should contain theSameElementsAs List(URBDeliver(0,payload.msg))
    }
  }

}
