package oss.ggiussi.cappio.impl.links

import oss.ggiussi.cappio.ProcessID
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

case class FairLossLink(a: ProcessID, b: ProcessID)(implicit payloads: Payloads) extends Automaton[FullFLLState] {
  override val sig: ActionSignature = {
    val in: Set[Action] = payloads.sends(a, b) ++ payloads.sends(b, a) ++ payloads.drops(a,b) ++ payloads.drops(b,a)
    val out: Set[Action] = payloads.delivers(a, b) ++ payloads.delivers(b, a)
    val int: Set[Action] = Set.empty[Action]
    ActionSignature(in = in, out = out, int = int)
  }

  override val steps: Steps.Steps[FullFLLState] =
    Steps.steps[FullFLLState]({
      case a@Drop(id) if inputAction(a) => Effect(_.drop(id))
      case a@Send(`a`, `b`, msg) if inputAction(a) => Effect(state => state.copy(ab = state.ab.add(msg)))
      case a@Deliver(`a`, `b`, msg) if outputAction(a) => Effect(state => state.ab.canDeliver(msg), state => state.copy(ab = state.ab.remove(msg)))

      case a@Send(`b`, `a`, msg) if inputAction(a) => Effect(state => state.copy(ba = state.ba.add(msg)))
      case a@Deliver(`b`, `a`, msg) if outputAction(a) => Effect(state => state.ba.canDeliver(msg), state => state.copy(ba = state.ba.remove(msg)))
    })
}