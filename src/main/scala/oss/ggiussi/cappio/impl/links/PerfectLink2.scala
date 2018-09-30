package oss.ggiussi.cappio.impl.links

import oss.ggiussi.cappio.impl.links.PLProtocol._
import oss.ggiussi.cappio._
import oss.ggiussi.cappio.impl.links.Protocol.{Envelope, ProcessID}


object PLProtocol {

  case class Send(from: ProcessID, to: ProcessID) extends Envelope

  case class Deliver(from: ProcessID, to: ProcessID) extends Envelope

}

object PerfectLink2 {

  def initial(from: ProcessID, to: ProcessID): (PerfectLink, PerfectLinkState) = (PerfectLink(from, to), PerfectLinkState(Set(), Set(), Set()))

}

case class PerfectLinkState2(sent: Set[Any], delivered: Set[Any]) {
  def send(message: Any) = copy(sent = sent + message)

  def canDeliver(message: Any): Boolean = (sent contains message) && !(delivered contains message)

  def deliver(message: Any) = if (canDeliver(message)) copy(delivered = delivered + message) else throw UnsatisfiedPreconditionException


}

case class PerfectLink2(from: ProcessID, to: ProcessID) extends Automaton[PerfectLinkState] {

  override val sig: ActionSignature = {
    val in: Set[Action] = Set(Send(from, to))
    val out: Set[Action] = Set(Deliver(from, to))
    val int: Set[Action] = Set()
    ActionSignature(in = in, out = out, int = int)
  }

  override val steps: Steps.Steps[PerfectLinkState] = Steps.steps({
    case Send(`from`, `to`) => Effect.inputEnabledP({ case (state, Some(message)) => state send message })
    case Deliver(`from`, `to`) => new Effect({ case (state, Some(message)) => state canDeliver message }, { case (state, Some(message)) => state deliver message })
  })
}