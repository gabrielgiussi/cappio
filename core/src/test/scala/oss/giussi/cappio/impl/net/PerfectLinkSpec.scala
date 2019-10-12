package oss.giussi.cappio.impl.net

import oss.giussi.cappio.impl.net.PerfectLink.{PLDeliver, PLSend}
import oss.giussi.cappio.{CappIOSpec, Instance, Packet}

class PerfectLinkSpec extends CappIOSpec {

  "A" should {
    "B" in {
      val packet = Packet(1, 0, "", Instance("SL"))
      PerfectLink.init(3)
        .request(PLSend(packet))
        .deliver(packet)
        .indications should contain theSameElementsAs Set(PLDeliver(packet))
    }
    "C" in {
      val packet = Packet(1, 0, "", Instance("SL"))
      PerfectLink.init(3)
        .request(PLSend(packet))
        .deliver(packet)
        .deliver(packet)
        .indications shouldBe empty
    }
  }

}
