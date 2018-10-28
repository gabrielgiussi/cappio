package oss.ggiussi.cappio.impl.links

import oss.ggiussi.cappio.{InstanceID, ProcessID}
import oss.ggiussi.cappio.core.LinkProtocol.{Deliver, Drop, Send}
import oss.ggiussi.cappio.core._

object PLState {
  def empty() = PLState(Set.empty, Set.empty)
}

case class PLState(sent: Set[Message], delivered: Set[Message]) {
  def send(message: Message): PLState = copy(sent = sent + message)

  def deliver(message: Message): PLState = copy(delivered = delivered + message)

  def canDeliver(message: Message): Boolean = (sent contains message) && !(delivered contains message)

  def isEmpty(): Boolean = (sent -- delivered) isEmpty
}

object FullPLState {
  def empty() = FullPLState(PLState.empty, PLState.empty)
}

case class FullPLState(ab: PLState, ba: PLState) {
  def isEmpty() = ab.isEmpty && ba.isEmpty
}

object PerfectLink {

  def fullDuplex(instance: InstanceID)(a: ProcessID, b: ProcessID)(implicit payloads: Payloads): Automaton[FullPLState] = PerfectLink(a, b, instance).compose(PerfectLink(b, a, instance), (ab, ba) => FullPLState(ab, ba))(_.ab, _.ba).get

  def apply(instance: InstanceID)(from: ProcessID, to: ProcessID)(implicit payloads: Payloads): PerfectLink = new PerfectLink(from, to, instance)(payloads)
}

case class PerfectLink(from: ProcessID, to: ProcessID, instance: InstanceID)(implicit payloads: Payloads) extends Automaton[PLState] {

  implicit val instanceID = instance

  override val sig: ActionSignature = {
    val in: Set[Action] = payloads.sends(from, to) //++ payloads.drops(from, to)
    val out: Set[Action] = payloads.delivers(from, to)
    val int: Set[Action] = Set.empty
    ActionSignature(in = in, out = out, int = int)
  }
  override val steps: Steps.Steps[PLState] = Steps.steps[PLState]({
    case a@Send(`from`, `to`, `instance`, msg) if inputAction(a) => Effect(_.send(msg))
    case a@Deliver(`from`, `to`, `instance`, msg) if outputAction(a) => Effect(_.canDeliver(msg), _.deliver(msg))
    //case a@Drop(_) if inputAction(a) => Effect.noEffect()
  })
}
