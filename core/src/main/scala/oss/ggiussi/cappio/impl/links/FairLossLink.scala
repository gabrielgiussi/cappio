package oss.ggiussi.cappio.impl.links

import oss.ggiussi.cappio.{InstanceID, ProcessID}
import oss.ggiussi.cappio.core.LinkProtocol._
import oss.ggiussi.cappio.core.Transition.Transition
import oss.ggiussi.cappio.core._

object FLLState {
  def empty(): FLLState = FLLState(Set.empty)

}

// TOOD should be something like Payload instead of Any (but Payloa dis Option[Any])
case class FLLState(messages: Set[Message]) {
  def add(message: Message): FLLState = copy(messages = messages + message)

  def remove(message: Message): FLLState = copy(messages = messages - message)

  def canDeliver(message: Message): Boolean = messages contains message

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
  def apply(instance: InstanceID)(a: ProcessID, b: ProcessID)(implicit payloads: Payloads): FairLossLink = new FairLossLink(a, b,instance)
}

case class FairLossLink(a: ProcessID, b: ProcessID, instance: InstanceID)(implicit payloads: Payloads) extends Automaton[FullFLLState] {
  implicit val instanceID = instance // TODO esto es propio de Automaton? (no es de la teoria pero lo voy a usar siempre)

  override val sig: ActionSignature = {
    val in: Set[Action] = payloads.sends(a, b) ++ payloads.sends(b, a) ++ payloads.drops(a, b) ++ payloads.drops(b, a)
    val out: Set[Action] = payloads.delivers(a, b) ++ payloads.delivers(b, a)
    val int: Set[Action] = Set.empty[Action]
    ActionSignature(in = in, out = out, int = int)
  }

  override val steps: Steps.Steps[FullFLLState] =
    Steps.steps[FullFLLState]({
      case a@Drop(id, `instance`) if inputAction(a) => Effect(_.drop(id))
      case a@Send(`a`, `b`, `instance`, msg) if inputAction(a) => Effect(state => state.copy(ab = state.ab.add(msg)))
      case a@Deliver(`a`, `b`, `instance`, msg) if outputAction(a) => Effect(state => state.ab.canDeliver(msg), state => state.copy(ab = state.ab.remove(msg)))

      case a@Send(`b`, `a`, `instance`, msg) if inputAction(a) => Effect(state => state.copy(ba = state.ba.add(msg)))
      case a@Deliver(`b`, `a`, `instance`, msg) if outputAction(a) => Effect(state => state.ba.canDeliver(msg), state => state.copy(ba = state.ba.remove(msg)))
    })
}