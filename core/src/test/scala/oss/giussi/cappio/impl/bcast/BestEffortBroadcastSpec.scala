package oss.giussi.cappio.impl.bcast

import org.scalatest.{Matchers, WordSpec}
import oss.giussi.cappio.{Instance, Packet, ProcessId}
import oss.giussi.cappio.impl.bcast.BestEffortBroadcast.BebBcast
import oss.giussi.cappio.impl.net.FairLossLink.FLLSend
import oss.giussi.cappio.impl.net.PerfectLink

class BestEffortBroadcastSpec extends WordSpec with Matchers {

  implicit def intToProcessId(id: Int) = ProcessId(id)

  "BestEfforBroadcast" must {
    "A" in {
      val instance = Instance("beb")
      BestEffortBroadcast(0,Set(0,1,2),PerfectLink.init(3))
        .request(BebBcast("something",instance))
        .send.map { case FLLSend(Packet(_,payload,from,to,_)) => (from.id,to.id,payload) } should contain theSameElementsAs Set(0,1,2).map(to => (0,to,"something"))

    }
  }

}
