package oss.ggiussi.cappio.ui.app

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import oss.ggiussi.cappio.{InstanceID, ProcessID}
import oss.ggiussi.cappio.core.{Action, Execution}
import oss.ggiussi.cappio.core.LinkProtocol._
import oss.ggiussi.cappio.impl.Instances
import oss.ggiussi.cappio.impl.processes.ProcessBcast
import oss.ggiussi.cappio.ui.app.ActionSelection.BcastProps

sealed trait ActionSelectionProps
case class SendSelection(instanceID: InstanceID) extends ActionSelectionProps
case class BCastSelection(instanceID: InstanceID, processes: Set[ProcessID]) extends ActionSelectionProps
case class CrashSelection(instanceID: InstanceID, processes: Set[ProcessID]) extends ActionSelectionProps

case class ExecutionProps(selection: List[ActionSelectionProps], next: Action => Callback, exec: Execution[_], last: Boolean, currentStep: Int)

object ExecutionComponent {

  def actionSelected(n: Action => Callback)(action: Action): Callback = n(action)

  def couldDrop(sched: List[Action], id: MessageID): Boolean = false

  /*
  def couldDrop(sched: List[Action], id: MessageID): Boolean = {
    val sent = sched.exists {
      case SendHeader(_,_,_,Message(_,`id`)) /*if id == i */ => true
      case _ => false
    }
    val dropped = sched.exists {
      case DropHeader(`id`,_) => true
      case _ => false
    }
    val delivered = sched.exists {
      case DeliverHeader(_,_,_,Message(_,`id`)) => true
      case _ => false
    }
    sent && !delivered && !dropped
  }
  */

  val ExecutionComponent =
    ScalaComponent.builder[ExecutionProps]("Execution")
      .render_P { case ExecutionProps(selection, next, exec, last, currentStep) =>
        <.div(
          selection.flatMap {
            case SendSelection(instanceID) => Some(ActionSelection.Send((instanceID, next)))
            case BCastSelection(instance,processes) => Some(ActionSelection.Bcast(BcastProps(instance,next,processes)))
            case CrashSelection(instance,processes) => Some(ActionSelection.CrashSelector((instance,processes,next)))
            case _ => None
          }.toVdomArray,
          ActionSelection.ActionList((exec.enabledActions,next))
        )
      }
      .build

  def apply(p: ExecutionProps) = ExecutionComponent.apply(p)

}
