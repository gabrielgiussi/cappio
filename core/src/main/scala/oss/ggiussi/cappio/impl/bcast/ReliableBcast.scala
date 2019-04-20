package oss.ggiussi.cappio.impl.bcast

import oss.ggiussi.cappio.core.LinkProtocol.{DeliverHeader, Message, SendHeader}
import oss.ggiussi.cappio.core._
import oss.ggiussi.cappio.impl.bcast.BrokenBcastProtocol.{BrkBcastHeader, BrkDeliver, BrkDeliverHeader}
import oss.ggiussi.cappio.impl.bcast.RbBcastProtocol.{RbDeliver, RbDeliverHeader}
import oss.ggiussi.cappio.impl.faildet.PerfectFailureDetectorProtocol.Crashed
import oss.ggiussi.cappio.impl.{Instances, Triggers}
import oss.ggiussi.cappio.{InstanceID, ProcessID}

object RbBcastProtocol {

  def bcast(from: ProcessID, instanceID: InstanceID, message: Message): Action = RbBcast(RbBcastHeader(from,instanceID),message)

  case class RbBcastHeader(from: ProcessID, instance: InstanceID) extends ActionHeader

  case class RbDeliverHeader(from: ProcessID, to: ProcessID, instance: InstanceID) extends ActionHeader

  case class RbDeliver(header: RbDeliverHeader, msg: Message) extends Action {
    override def payload = Some(msg)
  }

  case class RbBcast(header: RbBcastHeader, msg: Message) extends Action {
    override def payload = Some(msg)
  }


}

object RbBcastState {
  def init(instance: InstanceID)(self: ProcessID, neighbors: Set[ProcessID]) = RbBcastState(self,neighbors + self,neighbors + self map(_ -> Set.empty[Message]) toMap, Triggers.init(), instance)
}

case class RbBcastState(self: ProcessID, correct: Set[ProcessID], rbDelivered: Map[ProcessID,Set[Message]], triggers: Triggers, instance: InstanceID) {
  // TODO message or payload?
  def bcast(processes: Set[ProcessID], message: Message) = {
    val bebBcast = BrokenBcastProtocol.bcast(self,Instances.BCAST,message)
    NextState(copy(triggers = triggers.trigger(bebBcast)), Set(bebBcast))
  }

  def deliver(from: ProcessID, msg: Message) = {
    if (!rbDelivered(from).contains(msg)){
      val rbDeliver = RbDeliver(RbDeliverHeader(from,self,instance),msg)
      val delivered = rbDelivered.updated(from,rbDelivered(from) + msg)
      val triggered: Set[Action] = if (correct.contains(from)) Set(rbDeliver) else Set(rbDeliver,BrokenBcastProtocol.bcast(from,Instances.BCAST,msg)) // TODO refactor
      NextState(copy(triggers = triggers.trigger(triggered), rbDelivered = delivered), triggered)
    }
    else NextState(this)
  }

  def crashed(p: ProcessID): NextState[RbBcastState] = {
    val bcasts: Set[Action] = rbDelivered(p).map(BrokenBcastProtocol.bcast(p,Instances.BCAST,_)).toSet
    NextState(copy(triggers = triggers.trigger(bcasts), correct = correct - p))
  }

  def triggered(action: Action) = copy(triggers = triggers.trigger(action))
}

class ReliableBcast(id: ProcessID, neighbors: Set[ProcessID], instance: InstanceID) extends Automaton[RbBcastState] {

  import Instances._
  import RbBcastProtocol._

  private val processes = neighbors + id

  override val sig: ActionSignature = {
    val in: Set[ActionHeader] = processes.map(BrkDeliverHeader(_,id,BCAST)) ++ Set(RbBcastHeader(id, instance)) ++ neighbors.map(Crashed(_,FAILURE_DET_LINK))
    val out: Set[ActionHeader] = processes.map(RbDeliverHeader(id,_,instance)) ++ Set(BrkBcastHeader(id,BCAST))
    val int = Set.empty[ActionHeader]
    ActionSignature(in = in, out = out, int = int)
  }

  override val steps: Steps.Steps[RbBcastState] = Steps.steps2[RbBcastState](sig,{
    case RbBcast(_,msg) => Effect.triggers(_.bcast(processes, msg))
    case BrkDeliver(header,msg) => Effect.triggers(_.deliver(header.from,msg))
    case Crashed(p,_) => Effect.triggers(_.crashed(p))
    case action => Effect(_.triggers.wasTriggered(action), _.triggered(action))
  })

}
