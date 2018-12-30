package oss.ggiussi.cappio.impl.links

import oss.ggiussi.cappio.core.LinkProtocol._
import oss.ggiussi.cappio.core._
import oss.ggiussi.cappio.{InstanceID, ProcessID}

object PLState {
  def empty() = PLState(Set.empty, Set.empty)
}

case class PLState(sent: Set[Message], delivered: Set[Message]) {
  def send(message: Message): PLState = copy(sent = sent + message)

  def deliver(message: Message): PLState = copy(delivered = delivered + message)

  def canDeliver(message: Message): Boolean = (sent contains message) && !(delivered contains message)

  def canDrop(id: MessageID): Boolean = (sent.exists(_.id == id)) && !(delivered.exists(_.id == id))

  def isEmpty(): Boolean = (sent -- delivered) isEmpty
}

object FullPLState {
  def empty() = FullPLState(PLState.empty, PLState.empty)
}

case class FullPLState(ab: PLState, ba: PLState) {
  def isEmpty() = ab.isEmpty && ba.isEmpty
}

object PerfectLink {

  def fullDuplex(instance: InstanceID)(a: ProcessID, b: ProcessID): Automaton[FullPLState] = PerfectLink(a, b, instance).compose(PerfectLink(b, a, instance), (ab, ba) => FullPLState(ab, ba))(_.ab, _.ba).get

  def apply(instance: InstanceID)(from: ProcessID, to: ProcessID): PerfectLink = new PerfectLink(from, to, instance)
}

case class PerfectLink(from: ProcessID, to: ProcessID, instance: InstanceID) extends Automaton[PLState] {

  override val sig: ActionSignature = {
    val in: Set[ActionHeader] = Set(SendHeader(from,to,instance), DropHeader(from,to,instance))
    val out: Set[ActionHeader] = Set(DeliverHeader(from,to,instance))
    val int: Set[ActionHeader] = Set.empty
    ActionSignature(in = in, out = out, int = int)
  }
  override val steps: Steps.Steps[PLState] = Steps.steps2[PLState](sig,{
    case Send(_, msg) => Effect(_.send(msg))
    case Deliver(_,msg) => Effect(_.canDeliver(msg), _.deliver(msg))
    case Drop(_,id) if (from != to) => Effect.noEffect(_.canDrop(id))
  })
}