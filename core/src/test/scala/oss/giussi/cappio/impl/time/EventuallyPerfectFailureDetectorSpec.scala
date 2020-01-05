package oss.giussi.cappio.impl.time

import org.scalatest.{Matchers, WordSpec}
import oss.giussi.cappio._
import oss.giussi.cappio.impl.net.FairLossLink.FLLSend
import oss.giussi.cappio.impl.time.EventuallyPerfectFailureDetector.{EPFDMod, Restore, Suspect}

class EventuallyPerfectFailureDetectorSpec extends WordSpec with Matchers {

  "A" should {
    "b" in {
      val timeout = 3
      val all = (0 to 2).map(ProcessId).toSet
      val epfd: Module[EPFDMod] = EventuallyPerfectFailureDetector(all, timeout)(ProcessId(0))

      (1 to (timeout * 2) - 1).foldLeft(epfd)((a,_) => a.tick.module)
        .tick.indications should contain theSameElementsAs List(Suspect(ProcessId(1)),Suspect(ProcessId(2)))
    }
    "c" in {
      val timeout = 3
      val all = (0 to 2).map(ProcessId).toSet
      val epfd: Module[EPFDMod] = EventuallyPerfectFailureDetector(all, timeout)(ProcessId(0))

      (1 to (timeout * 2) - 1).foldLeft(epfd)((a,_) => a.tick.module)
        .tail.deliver(FLLDeliver(Packet(1,0,HeartbeatReply,null))).module
        .tick.indications should contain theSameElementsAs List(Suspect(ProcessId(2)))
    }
    "d" in {
      val timeout = 3
      val all = (0 to 2).map(ProcessId).toSet
      val epfd: Module[EPFDMod] = EventuallyPerfectFailureDetector(all, timeout)(ProcessId(0))

      val epfd0 = (1 to (timeout * 2)).foldLeft(epfd)((a,_) => a.tick.module)
        .tail.deliver(FLLDeliver(Packet(1,0,HeartbeatReply,null))).module

      val a = (1 to timeout - 1).foldLeft(epfd0)((a,_) => a.tick.module)
      a.tick.indications should contain theSameElementsAs List(Restore(ProcessId(1)))
    }

    "e" in {
      val timeout = 3
      val all = (0 to 2).map(ProcessId).toSet
      val epfd: Module[EPFDMod] = EventuallyPerfectFailureDetector(all, timeout)(ProcessId(0))
      (1 to (timeout * 2) - 1).foldLeft(epfd)((a,_) => a.tick.module)
        .tick.send
        .map { case FLLSend(p@Packet(_,_,from,to,_)) => (p.payload,from.id,to.id) } should contain theSameElementsAs List(1,2).map((HeartbeatRequest,0,_))
    }

    "ddd" in {
      val all = (0 to 2).map(ProcessId).toSet
      EventuallyPerfectFailureDetector(all, 3)(ProcessId(0))
        .tail.deliver(FLLDeliver(Packet(1,0,HeartbeatRequest,null)))
        .send.map { case FLLSend(p@Packet(_,_,from,to,_)) => (p.payload,from.id,to.id) } should contain theSameElementsAs List((HeartbeatReply,0,1))
    }
  }

}
