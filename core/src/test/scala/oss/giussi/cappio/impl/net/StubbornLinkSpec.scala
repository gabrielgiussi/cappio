package oss.giussi.cappio.impl.net

import oss.giussi.cappio.impl.net.FairLossLink.FLLSend
import oss.giussi.cappio.impl.net.StubbornLink.{SLDeliver, SLSend}
import oss.giussi.cappio.{CappIOSpec, Instance, Packet}


class StubbornLinkSpec extends CappIOSpec {

  "A" should {
    "B" in {
      val packet = Packet(1,0,"",Instance("SL"))
      StubbornLink.init(3)
        .request(SLSend(packet))
        .deliver(packet)
        .indications should contain theSameElementsAs Set(SLDeliver(packet))
    }
    "C" in {
      val packet = Packet(1,0,"",Instance("SL"))
      StubbornLink.init(2)
        .request(SLSend(packet))
        .tick
        .tick
        .send should contain theSameElementsAs List(FLLSend(packet))

    }
  }


}
