package oss.ggiussi.cappio.ui.app

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import oss.ggiussi.cappio.InstanceID
import oss.ggiussi.cappio.core.{Action, Execution}
import oss.ggiussi.cappio.core.LinkProtocol._
import oss.ggiussi.cappio.impl.Instances
import oss.ggiussi.cappio.impl.bcast.BrokenBcastProtocol.BrkBcastHeader
import oss.ggiussi.cappio.impl.processes.ProcessBcast
import oss.ggiussi.cappio.ui.app.ActionSelection.BcastProps

case class ExecutionProps(next: Action => Callback, exec: Execution[_], last: Boolean, currentStep: Int)

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
      .render_P { case ExecutionProps(next, exec, last, currentStep) =>
        <.div(
          ActionSelection.Send((InstanceID("a"), next)),
          ActionSelection.Bcast(BcastProps(Instances.BCAST,next,Set(0,1,2))),
          ActionSelection.CrashSelector((ProcessBcast.instance,Set(0,1,2),next)),
          ActionSelection.ActionList((exec.enabledActions,next)),
          <.label(
            s"Size ${exec.enabled.size}"
          )
        )
        /*
          <.ul(
            exec.enabled().filterNot {
              //case Send(_,_,Message(_,MessageID(_,_,_,step))) if step != currentStep => true
              case DropHeader(id,_) => !couldDrop(exec.sched,id)
              case BrkBcastHeader(_,_,_,step) if step != currentStep => true // TODO
              case _ => false
            }.zipWithIndex.toVdomArray { case (action, index) =>
              <.li(
                ^.onClick -->? Option(actionSelected(next)(action)).filter(_ => last),
                action.toString,
                ^.key := index
              )
            }
        )
        */
      }
      .build


  def apply(p: ExecutionProps) = ExecutionComponent.apply(p)

}
