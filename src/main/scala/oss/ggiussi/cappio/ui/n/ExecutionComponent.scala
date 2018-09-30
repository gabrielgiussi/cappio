package oss.ggiussi.cappio.ui.n

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import oss.ggiussi.cappio.core.Action
import oss.ggiussi.cappio.core.Execution
import oss.ggiussi.cappio.core.FLLProtocol.{Message, MessageID, Send}

case class ExecutionProps(next: Action => Callback, exec: Execution[_], last: Boolean, currentStep: Int)

object ExecutionComponent {

  def actionSelected(n: Action => Callback)(action:Action): Callback = n(action)

  val ExecutionComponent =
    ScalaComponent.builder[ExecutionProps]("Execution")
      .render_P { case ExecutionProps(next, exec, last, currentStep) =>
        //<.div(
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
         // )
        )
      }
      .build


  def apply(p: ExecutionProps) = ExecutionComponent.apply(p)

}
