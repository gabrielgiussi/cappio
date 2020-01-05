package oss.giussi.cappio.impl.time

import org.scalatest.{Matchers, WordSpec}
import oss.giussi.cappio._
import oss.giussi.cappio.impl.net.FairLossLink.FLLSend
import oss.giussi.cappio.impl.time.PerfectFailureDetector.PFDMod

class PerfectFailureDetectorSpec extends WordSpec with Matchers {
  "A" should {
    "a" in {
      val all = (0 to 2).map(ProcessId).toSet
      val pfd = PerfectFailureDetector(all, 3)(ProcessId(0))
      pfd
        .tick.module
        .tick.module
        .tick.send.map {
        case FLLSend(p@Packet(_, _, from, to, _)) => (p.payload, from.id, to.id)
      } should contain theSameElementsAs List((HeartbeatRequest,0,1),(HeartbeatRequest,0,2))
    }
    "b" in {
      val timeout = 3
      val all = (0 to 2).map(ProcessId).toSet
      val pfd: Module[PFDMod] = PerfectFailureDetector(all,timeout)(ProcessId(0))
      (1 to (timeout * 2) - 1).foldLeft(pfd)((a,_) => a.tick.module)
        .tick.indications should contain theSameElementsAs List(Crashed(ProcessId(1)),Crashed(ProcessId(2)))
    }
    "c" in {
      val timeout = 3
      val all = (0 to 2).map(ProcessId).toSet
      val pfd: Module[PFDMod] = PerfectFailureDetector(all,timeout)(ProcessId(0))
      (1 to (timeout * 2) - 1).foldLeft(pfd)((a,_) => a.tick.module)
        .tail.deliver(FLLDeliver(Packet(1,0,HeartbeatReply,null))).module
        .tick.indications should contain theSameElementsAs List(Crashed(ProcessId(2)))
    }
  }
}
