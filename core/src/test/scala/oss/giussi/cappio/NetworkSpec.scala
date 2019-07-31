package oss.giussi.cappio

import oss.giussi.cappio.impl.net.FairLossLink.FLLSend

class NetworkSpec extends CappIOSpec {

  val network = Network.init[String]

  val packetA = Packet(p0,p1,"A",Instance.ANY)
  val packetB = Packet(p1,p2,"B",Instance.ANY)

  val packets = Set(packetA,packetB)

  "Network" must {
    "return no in transit packets if the network is empty" in {
      network.inTransit shouldBe empty
    }
    "return in transit packets" in {
      network.send(packets.map(FLLSend.apply)).inTransit.map(_.packet) should contain theSameElementsAs packets
    }
    "drop packets" in {
      val n = network.send(packets.map(FLLSend.apply))
      val drop = n.inTransit.filter(_.packet == packetA).head.drop
      n.drop(Set(drop))
        .get.inTransit.map(_.packet) should contain theSameElementsAs Set(packetB)
    }
    "prepare a deliver for a packet" in {
      network.send(packets.map(FLLSend.apply))
        .inTransit.map(_.deliver.packet) should contain theSameElementsAs packets
    }
    "prepare a drop for a packet" in {
      network.send(packets.map(FLLSend.apply))
        .inTransit.map(_.drop.packet) should contain theSameElementsAs packets
    }
    "deliver a packet in transit" in {
      val n = network.send(packets.map(FLLSend.apply))
      val deliver = n.inTransit.filter(_.packet == packetA).head.deliver
      n.deliver(Set(deliver)).get.inTransit.map(_.packet) should contain theSameElementsAs Set(packetB)
    }
    // TODO this test shouldn't exist because drops and delivers should be created only by the network
    "fail when the packet to deliver is not in transit" in {
      network.send(Set(FLLSend(packetA))).deliver(Set(FLLDeliver(packetB))).isFailure shouldBe true
    }
    "fail when the packet to drop is not in transit" in {
      network.send(Set(FLLSend(packetA))).drop(Set(Drop(packetB))).isFailure shouldBe true
    }

  }

}
