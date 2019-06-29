package oss.giussi.cappio.impl.bcast

import oss.giussi.cappio.impl.bcast.CausalOrderReliableBroadcast.{CRBBroadcast, CRBData, CRBDeliver}
import oss.giussi.cappio.impl.bcast.ReliableBroadcast.RBData
import oss.giussi.cappio.impl.bcast.UniformReliableBroadcast.Payload
import oss.giussi.cappio.impl.net.FairLossLink.FLLSend
import oss.giussi.cappio.{CappIOSpec, Packet, ProcessId}

class CausalOrderReliableBroadcastSpec extends CappIOSpec {

  val all = (0 to 2).map(ProcessId)
  val self = all.head
  val crb = CausalOrderReliableBroadcast.init(self,all.toSet,3)

  "A" should {
    "b" in {
      val payload = Payload("gaga")
      val uuid = payload.id
      crb.request(CRBBroadcast(payload))
        .send.map { case FLLSend(Packet(`uuid`,RBData(ProcessId(0),CRBData(_,"gaga")),`self`,to, _)) => to.id } should contain theSameElementsAs Set(0,1,2)
    }
    "c" in {
      val payload = Payload("gaga")
      val step0 = crb.request(CRBBroadcast(payload))
      val deliver = step0.send.find(_.packet.to.id == 0).head.packet
      step0.deliver(deliver)
        .indications should contain theSameElementsAs Set(CRBDeliver(self,"gaga"))


    }
  }

}
