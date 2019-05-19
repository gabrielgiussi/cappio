package oss.giussi.cappio.impl.net

import org.scalatest.{Matchers, WordSpec}
import oss.giussi.cappio.impl.net.FairLossLink.FLLDeliver
import oss.giussi.cappio.impl.net.PerfectLink.{PLDeliver, PLSend}
import oss.giussi.cappio.{Instance, Packet}
import oss.giussi.cappio.impl.net.StubbornLink.{SLDeliver, SLSend}

class PerfectLinkSpec extends WordSpec with Matchers {

  "A" should {
    "B" in {
      val packet = Packet(1, 0, "", Instance("SL"))
      PerfectLinkBeta.init(3)
        .request(PLSend(packet))
        .module.tail.deliver(FLLDeliver(packet))
        .indications should contain theSameElementsAs Set(PLDeliver(packet))
    }
    "C" in {
      val packet = Packet(1, 0, "", Instance("SL"))
      PerfectLinkBeta.init(3)
        .request(PLSend(packet))
        .module.tail.deliver(FLLDeliver(packet))
        .module.tail.deliver(FLLDeliver(packet))
        .indications shouldBe empty
    }
  }

}
