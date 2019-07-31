package oss.giussi.cappio

import oss.giussi.cappio.impl.net.{FairLossLink, StubLink, StubbornLink}
import oss.giussi.cappio.impl.net.StubbornLink.{SLDeliver, SLSend}

class SchedulerSpec extends CappIOSpec {

  "Scheduler" should {
    "feed sent packets to the Network" in {
      val processes = Process(ALL,StubbornLink.init[String](3))
      val _0_to_1 = Packet(p0,p1,"p01",Instance("sl"))
      val _1_to_0 = Packet(p1,p0,"p10",Instance("sl"))
      Scheduler.init(processes)
        .req(p0 -> _0_to_1.slSend, p1 -> _1_to_0.slSend)
        .scheduler.network.inTransit.map(_.packet) should contain theSameElementsAs Set(_0_to_1,_1_to_0)
    }

    "trigger indications" in {
      val processes = Process(ALL,StubbornLink.init[String](3))
      val _0_to_1 = Packet(p0,p1,"p01",Instance("sl"))
      val _1_to_0 = Packet(p1,p0,"p10",Instance("sl"))
      Scheduler.init(processes)
        .req(p0 -> _0_to_1.slSend, p1 -> _1_to_0.slSend)
        .deliver(p1 -> _0_to_1, p0 -> _1_to_0)
        .indications should contain theSameElementsAs Set(IndicationFrom(p1,_0_to_1.slDeliver), IndicationFrom(p0, _1_to_0.slDeliver))
    }
  }

  "TickScheduler" should {
    "B" in {
      val processes = Process(ALL,StubbornLink.init[String](3))
      val packet = Packet(0,1,"pal",Instance("o"))
      WaitingRequest(TickScheduler(Scheduler.init(processes)))
        .request(RequestBatch(Map(ProcessId(0) -> SLSend(packet))))
        .deliver(DeliverBatch(Map(ProcessId(1) -> Left(FLLDeliver(packet)))))
        .ind should contain theSameElementsAs Set(IndicationFrom(ProcessId(1),packet.slDeliver))
    }
    "C" in  {
      val processes = Process(ALL,StubbornLink.init[String](3))
      val packet = Packet(0,1,"pal",Instance("o"))
      WaitingRequest(TickScheduler(Scheduler.init(processes)))
        .request(RequestBatch(Map(ProcessId(0) -> SLSend(packet))))
        .deliver(DeliverBatch(Map(ProcessId(1) -> Right(Drop(packet)))))
        .request(RequestBatch(Map.empty))
        .deliver(DeliverBatch(Map(ProcessId(1) -> Left(FLLDeliver(packet)))))
      // FIXME no testea nada
    }

  }


}
