package oss.ggiussi.cappio.impl

import org.scalatest.{Matchers, WordSpec}
import oss.ggiussi.cappio.InstanceID
import oss.ggiussi.cappio.core.Execution
import oss.ggiussi.cappio.core.LinkProtocol.Deliver
import oss.ggiussi.cappio.impl.faildet.PerfectFailureDetectorProtocol._
import oss.ggiussi.cappio.impl.faildet.{PFDState, PerfectFailureDetector}
import oss.ggiussi.cappio.impl.links.Message

class PerfectFailureDetectorSpec extends WordSpec with Matchers {

  val instance = InstanceID("pfd-spec")

  val automaton = PerfectFailureDetector(0, instance)(Set(1, 2), 5)

  val state = PFDState.init(0,2,instance)(Set(1, 2))

  val execution = Execution(automaton, state)


  println(execution
    .next(Deliver(1, 0, Instances.FAILURE_DET_LINK, Message(1, 0, HeartbeatRequest(1), 0)))
    .next(Timeout(instance, 0))
    .next(Deliver(1, 0, Instances.FAILURE_DET_LINK, Message(1, 0, HeartbeatReply(1), 0)))
    .sched())

  "Perfect Failure Detector" must {
    "trigger a Timeout after n ticks" in {
      execution.next(Tick).next(Tick).sched().take(3) shouldBe List(Tick,Tick,Timeout(instance,1))
    }
    "trigger a Crashed if doesn't receive a Reply from a process" in {
      execution.next(Tick).next(Tick).next(Tick).next(Tick).sched() should contain(Crashed(1,instance))
    }
  }


}
