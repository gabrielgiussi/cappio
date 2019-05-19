package oss.giussi.cappio.impl.time

import org.scalatest.{Matchers, WordSpec}
import oss.giussi.cappio.impl.net.FairLossLink.FLLDeliver
import oss.giussi.cappio.impl.time.LeaderElection.{LEState, Leader}
import oss.giussi.cappio.impl.time.PerfectFailureDetector.HeartbeatReply
import oss.giussi.cappio.{Module, NoRequest, Packet, ProcessId}

class LeaderElectionSpec extends WordSpec with Matchers {

  "B" should {
    "a" in {
      val timeout = 3
      val all = (0 to 2).map(ProcessId).toSet
      val le: Module[NoRequest,LEState,Leader] = LeaderElection.init(ProcessId(2),all,timeout)
      (1 to (timeout * 2) - 1).foldLeft(le)((a,_) => a.tick.module)
        .tick.indications should contain theSameElementsAs List(1,2).map(id => Leader(ProcessId(id)))
    }
    "b" in {
      /*
        Por que tengo que saber que tengo que devolver un Reply en este test. Dedeberia poder testear solo la implementacion "interna", ejemplo mandarle Crash
        y esperar Leader!
        Y dsp si tener algun test de integracion
       */
      val timeout = 3
      val all = (0 to 2).map(ProcessId).toSet
      val le: Module[NoRequest,LEState,Leader] = LeaderElection.init(ProcessId(2),all,timeout)
      (1 to (timeout * 2) - 1).foldLeft(le)((a,_) => a.tick.module)
        .tail.deliver(FLLDeliver(Packet(1,2,HeartbeatReply,null))).module
        .tick.indications should contain theSameElementsAs List(Leader(ProcessId(1)))
    }
  }

}
