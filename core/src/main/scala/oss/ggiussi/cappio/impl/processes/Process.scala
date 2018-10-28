package oss.ggiussi.cappio.impl.processes

import oss.ggiussi.cappio.{InstanceID, ProcessID}
import oss.ggiussi.cappio.core._
import oss.ggiussi.cappio.core.LinkProtocol._
import oss.ggiussi.cappio.impl.processes.ProcessProtocol.Crash

object ProcessProtocol {

  case class Crash(id: ProcessID, instance: InstanceID) extends Action

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

case class Process(id: ProcessID, neighbors: Set[ProcessID], instance: InstanceID)(implicit payloads: Payloads) extends Automaton[ProcessState] {

  implicit val instanceID = instance

  val processInstance = InstanceID("process") // TODO

  override val sig: ActionSignature = {
    val in: Set[Action] = {
      val delivers: Set[Action] = neighbors.flatMap(payloads.delivers(_, id))
      delivers + Crash(id, processInstance)
    }
    val out: Set[Action] = neighbors.flatMap(payloads.sends(id, _)) // FIXME send es una output action, porque esta en steps.
    val int: Set[Action] = Set.empty
    ActionSignature(in = in, out = out, int = int)
  }
  override val steps: Steps.Steps[ProcessState] = Steps.steps {
    case Send(`id`, _, `instance`, payload) if payloads.payloads contains payload.payload => Effect.precondition(_.isInstanceOf[Up])
    case Deliver(_, `id`, `instance`, payload) if payloads.payloads contains payload.payload => Effect(_.isInstanceOf[Up], _ deliver payload.payload)
    case Crash(`id`, `processInstance`) => Effect(_.isInstanceOf[Up], _ => Down)
  }

}
