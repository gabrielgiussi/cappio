package oss.ggiussi.cappio.impl.processes

import oss.ggiussi.cappio.{InstanceID, ProcessID, Processes}
import oss.ggiussi.cappio.core._
import oss.ggiussi.cappio.core.LinkProtocol._
import oss.ggiussi.cappio.impl.processes.ProcessProtocol.Crash

object ProcessProtocol {

  case class Crash(id: ProcessID, instance: InstanceID) extends SimpleAction

}

sealed trait ProcessState {

  def deliver(msg: Any): ProcessState

}

case class Up(x: Int) extends ProcessState {

  def deliver(msg: Any): ProcessState = copy(x = msg.toString.toInt) // TODO UGHHH

}

case object Down extends ProcessState {
  override def deliver(msg: Any): ProcessState = Down
}

// TODO instance es la instance del link
case class Process(id: ProcessID, instance: InstanceID)(implicit processes: Processes) extends Automaton[ProcessState] {

  val neighbors = processes.neighbors(id)

  val processInstance = InstanceID("process") // TODO

  override val sig: ActionSignature = {
    val in: Set[ActionHeader] = neighbors.map(DeliverHeader(_, id, instance)) ++ Set(Crash(id, processInstance))
    val out: Set[ActionHeader] = neighbors.map(SendHeader(id, _, instance))
    val int: Set[ActionHeader] = Set.empty
    ActionSignature(in = in, out = out, int = int)
  }
  override val steps: Steps.Steps[ProcessState] = Steps.steps2(sig, {
    case _: Send => Effect.precondition(_.isInstanceOf[Up])
    case Deliver(_, Message(payload, _)) => Effect(_.isInstanceOf[Up], _ deliver payload)
    case Crash(`id`, `processInstance`) => Effect(_.isInstanceOf[Up], _ => Down)
  })

}