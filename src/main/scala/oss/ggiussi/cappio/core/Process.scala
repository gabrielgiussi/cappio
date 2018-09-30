package oss.ggiussi.cappio.core

import oss.ggiussi.cappio.ProcessID
import oss.ggiussi.cappio.core.LinkProtocol._
import oss.ggiussi.cappio.core.ProcessProtocol.Crash

object ProcessProtocol {

  case class Crash(id: ProcessID) extends Action

}

sealed trait ProcessState {

  def deliver(msg: Any): ProcessState

}

case class Up(x: Int) extends ProcessState {

  def deliver(msg: Any): ProcessState = copy(x = msg.toString.toInt) // UGHHH

}

case object Down extends ProcessState {
  override def deliver(msg: Any): ProcessState = Down
}

case class ProcessN(id: ProcessID, neighbors: Set[ProcessID])(implicit payloads: Payloads) extends Automaton[ProcessState] {
  override val sig: ActionSignature = {
    val in: Set[Action] = {
      val delivers: Set[Action] = neighbors.map(payloads.delivers(_, id)).flatten //neighbors.flatMap(n => payloads.map(Deliver(n, id, _)))
      delivers + Crash(id)
    }
    val out: Set[Action] = Set.empty // FIXME send es una output action, porque esta en steps.
    val int: Set[Action] = Set.empty
    ActionSignature(in = in, out = out, int = int)
  }
  override val steps: Steps.Steps[ProcessState] = Steps.steps {
    case Send(`id`, _, payload) if payloads.payloads contains payload => Effect.precondition(_.isInstanceOf[Up])
    case Deliver(_, `id`, payload) if payloads.payloads contains payload => Effect(_.isInstanceOf[Up], _ deliver payload)
    case Crash(`id`) => Effect(_.isInstanceOf[Up], _ => Down)
  }

}
