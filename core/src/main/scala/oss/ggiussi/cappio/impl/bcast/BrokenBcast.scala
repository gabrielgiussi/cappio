package oss.ggiussi.cappio.impl.bcast

import oss.ggiussi.cappio.core.LinkProtocol._
import oss.ggiussi.cappio.core._
import oss.ggiussi.cappio.impl.{Instances, Triggers}
import oss.ggiussi.cappio.impl.bcast.BrokenBcastProtocol._
import oss.ggiussi.cappio.{InstanceID, ProcessID}

object BrokenBcastProtocol {

  def bcast(from: ProcessID, instance: InstanceID, message: Message, metadata: Option[Any] = None) = BrkBcast(BrkBcastHeader(from,instance),message)

  def bdeliver(from: ProcessID, to: ProcessID, instance: InstanceID, message: Message, metadata: Option[Any] = None) = BrkDeliver(BrkDeliverHeader(from,to,instance),message)

  case class BrkBcastHeader(from: ProcessID, instance: InstanceID) extends ActionHeader

  case class BrkDeliverHeader(from: ProcessID, to: ProcessID, instance: InstanceID) extends ActionHeader

  case class BrkDeliver(header: BrkDeliverHeader, msg: Message) extends Action {
    override def payload = Some(msg)
  }

  case class BrkBcast(header: BrkBcastHeader, msg: Message) extends Action {
    override def payload = Some(msg)
  }

}

object BrokenBcastState {
  //def empty() = BrokenBcastState(toSend = Set.empty, toDeliver = Set.empty)
  def empty() = BrokenBcastState(Triggers.init())
}

/*
case class BrokenBcastState(toSend: Set[Send], toDeliver: Set[BrkDeliver]) {

  def bcast(from: ProcessID, processes: Set[ProcessID], message: Message) = {
    val _toSend: Set[Send] = processes.map(to => LinkProtocol.send(from, to, Instances.BCAST_LINK,message))
    NextState(copy(toSend = toSend ++ _toSend), Set.empty[Action] ++ _toSend)
  }

  def canSend(s: Send): Boolean = toSend contains s

  def send(s: Send) = copy(toSend = toSend - s)

  def canBDeliver(d: BrkDeliver): Boolean = toDeliver contains d

  def bDeliver(d: BrkDeliver) = copy(toDeliver = toDeliver - d)

  def deliver(id: ProcessID, to: ProcessID, msg: Message) = {
    val _toDeliver = BrkDeliver(BrkDeliverHeader(id,to,Instances.BCAST), msg)
    NextState(copy(toDeliver = toDeliver + _toDeliver), Set(_toDeliver))
  }
}
*/
case class BrokenBcastState(triggers: Triggers) {
  // TODO message or payload?
  def bcast(from: ProcessID, processes: Set[ProcessID], message: Message) = {
    val toSend: Set[Action] = processes.map(to => LinkProtocol.send(from, to, Instances.BCAST_LINK,Message(message.payload)))
    NextState(copy(triggers.trigger(toSend)), Set.empty[Action] ++ toSend)
  }

  def deliver(id: ProcessID, to: ProcessID, msg: Message) = {
    val toDeliver = BrkDeliver(BrkDeliverHeader(id,to,Instances.BCAST), msg)
    NextState(copy(triggers.trigger(toDeliver)), Set(toDeliver))
  }

  def bDeliver(d: BrkDeliver) = copy(triggers.trigger(d))

  def send(s: Send) = copy(triggers.trigger(s))
}



object BrokenBcast {
  def apply(instance: InstanceID)(id: ProcessID, neighbors: Set[ProcessID]): BrokenBcast = new BrokenBcast(id, neighbors, instance)
}

// A BebBcast will look like the same, the only thing that changes is the implementation of the link
case class BrokenBcast(id: ProcessID, neighbors: Set[ProcessID], instance: InstanceID) extends Automaton[BrokenBcastState] {

  import Instances._

  private val processes = neighbors + id

  override val sig: ActionSignature = {
    val in: Set[ActionHeader] = processes.map(DeliverHeader(_,id,BCAST_LINK)) ++ Set(BrkBcastHeader(id, instance))
    val out: Set[ActionHeader] = processes.map(SendHeader(id,_,BCAST_LINK)) ++ processes.map(BrkDeliverHeader(_,id,instance))
    val int = Set.empty[ActionHeader]
    ActionSignature(in = in, out = out, int = int)
  }

  override val steps: Steps.Steps[BrokenBcastState] = Steps.steps2[BrokenBcastState](sig, {
    case BrkBcast(_,msg) => Effect.triggers(_.bcast(id, processes, msg))
    case a:BrkDeliver => Effect(_.triggers.wasTriggered(a), _.bDeliver(a))
    case a:Send => Effect(_.triggers.wasTriggered(a), _.send(a))
    case Deliver(DeliverHeader(from,_,_), msg) => Effect.triggers(_.deliver(from,id,msg))
  })
}
