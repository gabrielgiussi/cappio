package oss.giussi.cappio

import oss.giussi.cappio.impl.net.StubbornLink.SLSend
import oss.giussi.cappio.impl.net.{Socket, StubbornLink}

class SchedulerSpec extends CappIOSpec {

  val ALL_SET = ALL.toSet

  "Scheduler" should {
    val scheduler =  Scheduler.init(Process(ALL_SET,StubbornLink.init[String](3)))

    "feed sent packets to the Network" in {
      val _0_to_1 = Packet(p0,p1,"p01",Instance("sl"))
      val _1_to_0 = Packet(p1,p0,"p10",Instance("sl"))
      scheduler
        .req(p0 -> _0_to_1.slSend, p1 -> _1_to_0.slSend)
        .scheduler.network.inTransit.map(_.packet) should contain theSameElementsAs Set(_0_to_1,_1_to_0)
    }

    "trigger indications" in {
      val _0_to_1 = Packet(p0,p1,"p01",Instance("sl"))
      val _1_to_0 = Packet(p1,p0,"p10",Instance("sl"))
      scheduler
        .req(p0 -> _0_to_1.slSend, p1 -> _1_to_0.slSend) // con este codigo yo puedo testear p0 -> Packet(p1,p2)
        .deliver(_0_to_1.asId, _1_to_0.asId)
        .indications should contain theSameElementsAs Set(IndicationFrom(p1,_0_to_1.slDeliver), IndicationFrom(p0, _1_to_0.slDeliver))
    }

    "drop packets" in {
      val _0_to_1 = p0 -->> (p1, "p01")
      scheduler
        .req(p0 -> _0_to_1.slSend)
        .drop(_0_to_1.asId)
        .network.inTransit shouldBe empty
    }

    "crash a process" in {
      val allUp = ALL.map(_ -> Up).toMap
      scheduler.processes.mapValues(_.status) shouldBe allUp
      scheduler.crash(p2).processes.mapValues(_.status) shouldBe allUp + (p2 -> Down)
    }

    "not forward request to crashed processes" in {
      val _2_to_1 = Packet(p2,p1,"p21",Instance("sl"))
      val sch1 = scheduler.crash(p2)
      sch1.network.inTransit shouldBe empty
      sch1
        .req(p2 -> _2_to_1.slSend)
        .network.inTransit shouldBe empty
    }
  }

  "TickScheduler" should {
    "alternate requests and delivers" in {
      val processes = Process(ALL_SET,StubbornLink.init[String](3))
      val packet = Packet(p0,p1,"pal",Instance("o"))
      WaitingRequest(TickScheduler(Scheduler.init(processes)))
        .request(Seq(p0 --> SLSend(packet)))
        .deliver(DeliverBatch(Map(p1 -> Left(FLLDeliver(packet)))))
        .ind should contain theSameElementsAs Set(IndicationFrom(p1,packet.slDeliver))
    }
    "send tick after each step" in  {
      trait M extends Mod {
        override type Req = Unit
        override type Ind = String
        override type State = Int
      }
      case class TestModule(state: Int) extends Module[M] { self =>
        override def request(in: NoState): Next = next(self)

        override def tail: Socket[M] = (_: FLLDeliver[M#Payload]) => next(self)

        override def tick: Next = {
          val ns = state + 1
          if (ns == 5) next(copy(0), Set("ok"))
          else next(copy(ns))
        }
      }

      val processes = Process(ALL_SET,TestModule(0))
      WaitingRequest(TickScheduler(Scheduler.init(processes)))
        .request(Seq.empty)
        .deliver(DeliverBatch.empty)
        .request(Seq.empty)
        .deliver(DeliverBatch.empty)
        .request(Seq.empty)
        .ind shouldBe ALL_SET.map(IndicationFrom(_,"ok"))

    }

  }


}
