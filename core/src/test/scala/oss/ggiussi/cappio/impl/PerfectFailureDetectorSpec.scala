package oss.ggiussi.cappio.impl
import org.scalatest.{Matchers, WordSpec}
import oss.ggiussi.cappio.{InstanceID, Processes}
import oss.ggiussi.cappio.core.{Execution, LinkProtocol}
import oss.ggiussi.cappio.core.LinkProtocol.{Deliver, DeliverHeader, Message}
import oss.ggiussi.cappio.impl.faildet.PerfectFailureDetectorProtocol._
import oss.ggiussi.cappio.impl.faildet.{PFDState, PerfectFailureDetector}

class PerfectFailureDetectorSpec extends WordSpec with Matchers {

  val instance = InstanceID("pfd-spec")

  val automaton = PerfectFailureDetector(0, instance)(Processes(Set(0,1,2)))

  val state = PFDState.init(0,2,instance)(Set(1, 2))

  val execution = Execution(automaton, state)


  println(execution
    .next(LinkProtocol.deliver(1, 0,Message(HeartbeatRequest(1)), Instances.FAILURE_DET_LINK))
    .next(Timeout(instance))
    .next(LinkProtocol.deliver(1, 0, Message(HeartbeatReply(1)), Instances.FAILURE_DET_LINK))
    .sched())

  "Perfect Failure Detector" must {
    "trigger a Timeout after n ticks" in {
      execution.next(Tick).next(Tick).sched().take(3) shouldBe List(Tick,Tick,Timeout(instance))
    }
    "trigger a Crashed if doesn't receive a Reply from a process" in {
      execution.next(Tick).next(Tick).next(Tick).next(Tick).sched() should contain(Crashed(1,instance))
    }
  }


}