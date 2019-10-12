package oss.giussi.cappio.impl.bcast

import oss.giussi.cappio.impl.bcast.BestEffortBroadcast.{BebBcast, BebDeliver, BebMod}
import oss.giussi.cappio._

class BestEffortBroadcastSpec extends CappIOSpec with SchedulerSupport[BebMod[String]] {

  val instance = Instance.ANY
  val all = ALL.take(3).toSet
  val self = p0
  val beb = BestEffortBroadcast[String](all.toSet,3)(self)

  "BestEffortBroadcast" must {
    "Send the payload to all processes (including itsself)" in {
      beb.request(BebBcast(Payload("something"), instance))
        .packets.map(p => (p.from, p.to, p.payload)) should contain theSameElementsAs all.map(to => (self, to, "something"))
    }
    "Trigger a BebDeliver when packet is delivered" in {
      val packet = Packet(p0,p1,"s",instance)
      beb.tail.deliver(FLLDeliver(packet))
        .indications should contain theSameElementsAs List(BebDeliver(0.id,Payload(packet.id,"s")))
    }
  }

  val scheduler = Scheduler.init(all, BestEffortBroadcast[String](all,3))


  "BestEffortBroadcast cluster" must {
    "broadcast msg" in {
      val msg = "A"
      val payload = Payload(msg)
      schedule(scheduler,List(
        req(p0,BebBcast(payload,instance)),
        deliver(PacketId(p0,p0,msg)),
        deliver(PacketId(p0,p1,msg)),
        deliver(PacketId(p0,p2,msg))
      )) should contain theSameElementsInOrderAs List(p0,p1,p2).map(to => IndicationFrom(to,BebDeliver(p0,payload)))
    }

    "broadcast msg even if some packets are lost" in {
      val msg = "A"
      val payload = Payload(msg)
      schedule(scheduler,List(
        req(p0,BebBcast(payload,instance)),
        deliver(PacketId(p0,p0,msg)),
        deliver(PacketId(p0,p1,msg)),
        drop(PacketId(p0,p2,msg)),
        tick,
        tick,
        deliver(PacketId(p0,p2,msg))
      )) should contain theSameElementsInOrderAs List(p0,p1,p2).map(to => IndicationFrom(to,BebDeliver(p0,payload)))
    }
  }

}
