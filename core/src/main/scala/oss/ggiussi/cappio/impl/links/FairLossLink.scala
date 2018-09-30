package oss.ggiussi.cappio.impl.links

import oss.ggiussi.cappio.ProcessID
import oss.ggiussi.cappio.core.LinkProtocol._
import oss.ggiussi.cappio.core.Transition.Transition
import oss.ggiussi.cappio.core._

object FLLState {
  def empty(): FLLState = FLLState(Set.empty)

}

// TOOD should be something like Payload instead of Any (but Payloa dis Option[Any])
case class FLLState(messages: Set[Any]) {
  def add(message: Any): FLLState = copy(messages = messages + message)

  def remove(message: Any): FLLState = copy(messages = messages - message)

  def canDeliver(message: Any): Boolean = messages contains message
}

case class FairLossLink(from: ProcessID, to: ProcessID)(implicit payloads: Payloads) extends Automaton[FLLState] {
  override val sig: ActionSignature = {
    val in: Set[Action] = payloads.sends(from, to)
    val out: Set[Action] = payloads.delivers(from, to)
    val int: Set[Action] = Set.empty[Action]
    ActionSignature(in = in, out = out, int = int)
  }
  override val steps: Steps.Steps[FLLState] = {
    val transitions: Transition[FLLState] = {
      case Send(`from`, `to`, payload) if payloads.payloads contains payload.payload => Effect(state => state.add(payload))
      case Deliver(`from`, `to`, payload) if payloads.payloads contains payload.payload => Effect(state => state.canDeliver(payload), state => state.remove(payload))
    }
    Steps.steps(transitions)
  }
}