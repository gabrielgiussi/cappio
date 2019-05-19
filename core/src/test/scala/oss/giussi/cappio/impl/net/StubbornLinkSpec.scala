package oss.giussi.cappio.impl.net

import org.scalatest.{FlatSpec, Matchers, WordSpec}
import oss.giussi.cappio.{Instance, Packet, ProcessId, Tick}
import oss.giussi.cappio.impl.net.FairLossLink.{FLLDeliver, FLLSend}
import oss.giussi.cappio.impl.net.StubbornLink.{SLDeliver, SLSend}


class StubbornLinkSpec extends WordSpec with Matchers {

  "A" should {
    "B" in {
      val packet = Packet(1,0,"",Instance("SL"))
      StubbornLink.init(3)
        .request(SLSend(packet))
        .module.tail.deliver(FLLDeliver(packet))
        .indications should contain theSameElementsAs Set(SLDeliver(packet))
    }
    "C" in {
      val packet = Packet(1,0,"",Instance("SL"))
      StubbornLink.init(2)
        .request(SLSend(packet))
        .module
        .tick
        .module
        .tick
        .send should contain theSameElementsAs List(FLLSend(packet))

    }
  }


}
