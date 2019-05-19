package oss.giussi.cappio.impl

import org.scalatest.{Matchers, WordSpec}
import oss.giussi.cappio.impl.net.FairLossLink.FLLDeliver
import oss.giussi.cappio.impl.net.StubbornLink
import oss.giussi.cappio.impl.net.StubbornLink.{SLDeliver, SLSend}
import oss.giussi.cappio._

class SchedulerSpec extends WordSpec with Matchers {

  "Scheduler" should {
    "A" in {
      val processes = (0 to 2).map { id =>
        Process(ProcessId(id),StubbornLink.init(3))
      }.toList
      val packet = Packet(0,1,"p",Instance("sl"))
      val actions = RequestBatch(Map(ProcessId(0) -> SLSend(packet)))
      Scheduler.init(processes)
        .request(actions)
        .scheduler
        .deliver(DeliverBatch(Map(ProcessId(1) -> Left(FLLDeliver(packet)))))
        .indications.head shouldBe SLDeliver(packet)

    }
  }

  "TickScheduler" should {
    "B" in {
      val processes = (0 to 2).map(id => Process(ProcessId(id),StubbornLink.init(3))).toList
      val packet = Packet(0,1,"pal",Instance("o"))
      WaitingRequest(TickScheduler(Scheduler.init(processes)))
        .request(RequestBatch(Map(ProcessId(0) -> SLSend(packet))))
        ._2
        .deliver(DeliverBatch(Map(ProcessId(1) -> Left(FLLDeliver(packet)))))
        ._1 should contain theSameElementsAs List(SLDeliver(packet))
    }
    "C" in {
      val processes = (0 to 2).map(id => Process(ProcessId(id),StubbornLink.init(3))).toList
      val packet = Packet(0,1,"pal",Instance("o"))
      WaitingRequest(TickScheduler(Scheduler.init(processes)))
        .request(RequestBatch(Map(ProcessId(0) -> SLSend(packet))))
        ._2
        .deliver(DeliverBatch(Map(ProcessId(1) -> Right(Drop(packet)))))
        ._2
        .request(RequestBatch(Map.empty))
        ._2
        .deliver(DeliverBatch(Map(ProcessId(1) -> Left(FLLDeliver(packet)))))
    }
  }

}
