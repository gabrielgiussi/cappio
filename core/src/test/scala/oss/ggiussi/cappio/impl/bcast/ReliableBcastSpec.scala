package oss.ggiussi.cappio.impl.bcast

import org.scalatest.{Matchers, WordSpec}
import oss.ggiussi.cappio.InstanceID
import oss.ggiussi.cappio.core.Execution
import oss.ggiussi.cappio.core.LinkProtocol.Message
import oss.ggiussi.cappio.impl.Instances
import oss.ggiussi.cappio.impl.bcast.RbBcastProtocol.{RbBcast, RbBcastHeader, RbDeliver, RbDeliverHeader}

class ReliableBcastSpec extends WordSpec with Matchers {

  val instance = InstanceID("rb-bcast-spec")

  val automaton = new ReliableBcast(0, Set(1,2), instance)

  val state = RbBcastState.init(instance)(0,Set(1,2))

  val execution = Execution(automaton, state)

  "RbBcastSpect" must {
    "a" in {
      val msg = Message(1)
      execution.next(RbBcastProtocol.bcast(0,instance,msg))
        .next(BrokenBcastProtocol.bdeliver(0,0,Instances.BCAST,msg))
        .sched() should contain theSameElementsInOrderAs List(
        RbBcast(RbBcastHeader(0,instance),msg),
        BrokenBcastProtocol.bcast(0,Instances.BCAST,msg),
        BrokenBcastProtocol.bdeliver(0,0,Instances.BCAST,msg),
        RbDeliver(RbDeliverHeader(0,0,instance),msg)
      )
    }
  }

}
