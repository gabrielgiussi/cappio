package oss.giussi.cappio

import org.scalatest.{Matchers, WordSpec}
import oss.giussi.cappio.impl.net.StubbornLink
import oss.giussi.cappio.impl.net.StubbornLink.{SLDeliver, SLSend}

class SchedulerSpec extends CappIOSpec {

  "Scheduler" should {
    "A" in {
      val processes = (0 to 2).map { id =>
        Process(ProcessId(id),StubbornLink.init[String](3))
      }.toList
      val packet = Packet(0,1,"p",Instance("sl"))
      val actions = RequestBatch(Map(ProcessId(0) -> SLSend(packet)))
      Scheduler.init(processes)
        .request(actions)
        .scheduler
        .deliver(DeliverBatch(Map(ProcessId(1) -> Left(FLLDeliver(packet)))))
        .indications.head shouldBe IndicationFrom(ProcessId(1), SLDeliver(packet))

    }
  }

  "TickScheduler" should {
    "B" in {
      val processes = (0 to 2).map(id => Process(ProcessId(id),StubbornLink.init[String](3))).toList
      val packet = Packet(0,1,"pal",Instance("o"))
      WaitingRequest(TickScheduler(Scheduler.init(processes)))
        .request(RequestBatch(Map(ProcessId(0) -> SLSend(packet))))
        ._3
        .deliver(DeliverBatch(Map(ProcessId(1) -> Left(FLLDeliver(packet)))))
        ._2.head shouldBe IndicationFrom(ProcessId(1),SLDeliver(packet))
    }
    "C" in  {
      val processes = (0 to 2).map(id => Process(ProcessId(id),StubbornLink.init[String](3))).toList
      val packet = Packet(0,1,"pal",Instance("o"))
      WaitingRequest(TickScheduler(Scheduler.init(processes)))
        .request(RequestBatch(Map(ProcessId(0) -> SLSend(packet))))
        ._3
        .deliver(DeliverBatch(Map(ProcessId(1) -> Right(Drop(packet)))))
        ._3
        .request(RequestBatch(Map.empty))
        ._3
        .deliver(DeliverBatch(Map(ProcessId(1) -> Left(FLLDeliver(packet)))))
      // FIXME no testea nada
    }
  }

}
