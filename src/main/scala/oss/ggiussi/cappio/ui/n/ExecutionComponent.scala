package oss.ggiussi.cappio.ui.n

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import oss.ggiussi.cappio.core.{Action, Execution}
import oss.ggiussi.cappio.core.LinkProtocol.Send
import oss.ggiussi.cappio.impl.links.{Message, MessageID}

case class ExecutionProps(next: Action => Callback, exec: Execution[_], last: Boolean, currentStep: Int)

object ExecutionComponent {

  def actionSelected(n: Action => Callback)(action:Action): Callback = n(action)

  val ExecutionComponent =
    ScalaComponent.builder[ExecutionProps]("Execution")
      .render_P { case ExecutionProps(next, exec, last, currentStep) =>
          <.ul(
            exec.enabled().filterNot {
              case Send(_,_,Message(_,MessageID(_,_,_,step))) if step != currentStep => true
              case _ => false
            }.zipWithIndex.toVdomArray { case (action, index) =>
              <.li(
                ^.onClick -->? Option(actionSelected(next)(action)).filter(_ => last),
                action.toString,
                ^.key := index
              )
            }
        )
      }
      .build


  def apply(p: ExecutionProps) = ExecutionComponent.apply(p)

}
