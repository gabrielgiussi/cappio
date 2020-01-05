package oss.giussi.cappio.impl.register

import oss.giussi.cappio.Messages.{LocalRequest, LocalStep, PublicRequest}
import oss.giussi.cappio.impl.bcast.BestEffortBroadcast.BebBcast
import oss.giussi.cappio.impl.register.OneNRegularRegister._
import oss.giussi.cappio.{CappIOSpec, Packet, Payload, StateWithModule}
import shapeless.{Inl, Inr}

class OneNRegularRegisterSpec extends CappIOSpec {

  val all = ALL.take(3).toSet
  val timeout = 100

  val onrr = OneNRegularRegister[Int](all,timeout)(p0)

  "OneNRegularRegister" should {
    "Broadcast ONREAD" in {
      onrr.request(ONRRRead)
        .simplePackets should contain theSameElementsAs all.map(to => $p(p0,to,Inl(ONREAD(1))))
    }

    "Broadcast ONWRITE" in {
      onrr.request(ONRRWrite(1)).
        simplePackets should contain theSameElementsAs all.map(to => $p(p0,to,Inl(ONWRITE(1,1))))
    }

    "Trigger a ONRRWriteReturn when all the process acknoledge the write" in {
      onrr.request(ONRRWrite(1))
        .deliver(Packet(p1, p0, Inr(Inl(ONACK(1))), OneNRegularRegister.PL))
        .deliver(Packet(p2, p0, Inr(Inl(ONACK(1))), OneNRegularRegister.PL))
        .indications shouldBe Set(ONRRWriteReturn)
    }
    "Trigger a ONRRReadReturn with the value with highest timestamp" in {
      onrr.request(ONRRWrite(1))
        .deliver(Packet(p0, p0, Inr(Inl(ONACK(1))), OneNRegularRegister.PL))
        .deliver(Packet(p1, p0, Inr(Inl(ONACK(1))), OneNRegularRegister.PL))
        .request(ONRRRead)
        .deliver(Packet(p0, p0, Inr(Inl(ONVALUE(1, 1, Some(1)))), OneNRegularRegister.PL))
        .deliver(Packet(p1, p0, Inr(Inl(ONVALUE(1, 2, Some(5)))), OneNRegularRegister.PL))
        .indications shouldBe Set(ONRRReadReturn(5))
    }

    "F" in {
      val f = OneNRegularRegister.processLocal[Int](all.size, p0)
      val init = ONRRState.init[Int](p0, all, Int.MaxValue)
      val LocalStep(indications, events, requests, sends, StateWithModule(_,ONRRState(state))) = f(PublicRequest(ONRRRead), init)
      indications should be(empty)
      events should be(empty)
      sends should be(empty)
      requests.toList should matchPattern { case LocalRequest(Inl(BebBcast(Payload(_, ONREAD(1)), OneNRegularRegister.BEB))) :: Nil => }
      state shouldBe ONRRStateI(None, 0, 0, 0, 1, Map.empty)
    }
  }

}
