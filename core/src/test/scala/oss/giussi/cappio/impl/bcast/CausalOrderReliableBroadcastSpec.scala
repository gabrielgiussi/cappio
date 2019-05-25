package oss.giussi.cappio.impl.bcast

import org.scalatest.{Matchers, WordSpec}
import oss.giussi.cappio.{Packet, ProcessId}
import oss.giussi.cappio.impl.bcast.CausalOrderReliableBroadcast.{CRBBroadcast, CRBData, CRBDeliver}
import oss.giussi.cappio.impl.bcast.ReliableBroadcast.RBData
import oss.giussi.cappio.impl.bcast.UniformReliableBroadcast.Payload
import oss.giussi.cappio.impl.net.FairLossLink.FLLSend

class CausalOrderReliableBroadcastSpec extends WordSpec with Matchers {

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
      val deliver = step0.send.find(_.packet.to.id == 0).head.asDeliver
      step0.module.tail.deliver(deliver)
        .indications should contain theSameElementsAs Set(CRBDeliver(self,"gaga"))


    }
  }

}
