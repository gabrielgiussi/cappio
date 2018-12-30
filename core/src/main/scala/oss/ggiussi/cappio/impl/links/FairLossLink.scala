package oss.ggiussi.cappio.impl.links

import oss.ggiussi.cappio.{InstanceID, ProcessID}
import oss.ggiussi.cappio.core.LinkProtocol._
import oss.ggiussi.cappio.core._

object FLLState {
  def empty(): FLLState = FLLState(Set.empty)

}

// TOOD should be something like Payload instead of Any (but Payloa dis Option[Any])
case class FLLState(messages: Set[Message]) {
  def add(message: Message): FLLState = copy(messages = messages + message)

  def remove(message: Message): FLLState = copy(messages = messages - message)

  def canDeliver(message: Message): Boolean = messages contains message

  def canDrop(id: MessageID): Boolean = messages map (_.id) contains id

  def empty(): Boolean = messages.isEmpty

  def drop(id: MessageID): FLLState = copy(messages = messages.filterNot(_.id == id))
}

object FullFLLState {

  def empty() = FullFLLState(FLLState.empty, FLLState.empty)
}

case class FullFLLState(ab: FLLState, ba: FLLState) {
  def empty(): Boolean = ab.empty && ba.empty

  def drop(id: MessageID) = copy(ab = ab.drop(id), ba = ba.drop(id))
}

object FairLossLink {
  def apply(instance: InstanceID)(a: ProcessID, b: ProcessID): FairLossLink = new FairLossLink(a, b,instance)

  def fullDuplex(instance: InstanceID)(a: ProcessID, b: ProcessID): Automaton[FullFLLState] = FairLossLink(a, b, instance).compose(FairLossLink(b, a, instance), (ab, ba) => FullFLLState(ab, ba))(_.ab, _.ba).get

}

case class FairLossLink(a: ProcessID, b: ProcessID, instance: InstanceID) extends Automaton[FLLState] {

  import LinkProtocol._

  override val sig: ActionSignature = {
    val in: Set[ActionHeader] = Set(SendHeader(a, b,instance), DropHeader(a,b,instance))
    val out: Set[ActionHeader] = Set(DeliverHeader(a,b,instance))
    val int: Set[ActionHeader] = Set.empty
    ActionSignature(in = in, out = out, int = int)
  }

  override val steps =
    Steps.steps2[FLLState](sig, {
      case Send(_, msg) => Effect(_.add(msg))
      case Deliver(_, msg) => Effect(_.canDeliver(msg), _.remove(msg))
      case Drop(_,id) if (a != b) => Effect(_.canDrop(id), _.drop(id))
    })
}