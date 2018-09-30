package oss.ggiussi.cappio.impl.links
/*
import oss.ggiussi.cappio._
import oss.ggiussi.cappio.Transition.Transition
import oss.ggiussi.cappio.impl.links.FLLProtocol.{Deliver, Send}
import oss.ggiussi.cappio.impl.links.Protocol.ProcessID


object FLLProtocol {
  trait Envelope extends Action

  case class Send(from: ProcessID, to: ProcessID) extends Envelope

  case class Deliver(from: ProcessID, to: ProcessID) extends Envelope

}


// TOOD should be something like Payload instead of Any (but Payloa dis Option[Any])
case class FairLossLinkState2(messages: Set[Any]) {
  def add(message: Any): FairLossLinkState2 = copy(messages = messages + message)

  def remove(message: Any): FairLossLinkState2 = copy(messages = messages - message)

  def canDeliver(message: Any): Boolean = messages contains message
}

case class FairLossLink2(from: ProcessID, to: ProcessID) extends Automaton[FairLossLinkState2] {
  override val sig: ActionSignature = {
    val in: Set[Action] = Set(Send(from,to))
    val out: Set[Action] = Set(Deliver(from,to))
    val int: Set[Action] = Set.empty[Action]
    ActionSignature(in = in, out = out, int = int)
  }
  override val steps: Steps.Steps[FairLossLinkState2] = {
    val transitions: Transition[FairLossLinkState2] = {
      case Send(`from`, `to`) => Effect.inputEnabledP({ case (state,payload) => payload.map(state.add(_)).getOrElse(state) })
      case Deliver(`from`, `to`) => new Effect({ case (state,payload) => state.canDeliver(payload.get) },{ case (state,payload) => state.remove(payload.get) }) // payload.get
    }
    Steps.steps(transitions)
  }
}
*/