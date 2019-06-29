package oss.giussi.cappio.impl.bcast

import org.scalatest.{Matchers, WordSpec}
import oss.giussi.cappio.impl.bcast.ReliableBroadcast.{RBBcast, RBData, RBDeliver}
import oss.giussi.cappio.impl.bcast.UniformReliableBroadcast.Payload
import oss.giussi.cappio.impl.net.FairLossLink.FLLSend
import oss.giussi.cappio.{Packet, ProcessId}

class ReliableBroadcastSpec extends WordSpec with Matchers {

  val all = (0 to 2).map(ProcessId)
  val self = all.head
  val rb = ReliableBroadcast.init(self,all.toSet,3)

  "A" should {
    "a" in {
      val payload = Payload("gaga")
      val uuid = payload.id
      rb.request(RBBcast(payload))
        .send.map { case FLLSend(Packet(`uuid`,RBData(_,"gaga"),ProcessId(0),to,_)) => to.id } should contain theSameElementsAs Set(0,1,2)

    }
    "b" in {
      val payload = Payload("gaga")
      val step0 = rb.request(RBBcast(payload))
      val deliver = step0.send.find(_.packet.to.id == 0).get.asDeliver
      step0.module.tail.deliver(deliver)
        .indications should contain theSameElementsAs Set(RBDeliver(self,payload))
    }
  }

}
