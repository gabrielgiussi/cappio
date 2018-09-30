package oss.ggiussi.cappio.ui

import japgolly.scalajs.react._
import japgolly.scalajs.react.component.Scala.BackendScope
import japgolly.scalajs.react.vdom.html_<^._
import oss.ggiussi.cappio.impl.links.Protocol3.PLSend
import oss.ggiussi.cappio.{Do, Execution}
import oss.ggiussi.cappio.ui.ExecutionBackend.State
import oss.ggiussi.cappio.ui.ExecutionList.RoundsListProps

object ReactExecution {


  val ExecutionComponent =
    ScalaComponent.builder[Execution[_]]("Execution")
      .render_P(e => <.label(s"Steps: ", e.sched().length))
      .build

}

object ExecutionBackend {

  case class State(executions: List[Execution[_]]) {
    def next(): State = copy(executions = executions :+ executions.last.next(Do(PLSend(0, 1), Some(1))))
  }

  def apply(e: Execution[_]) = ScalaComponent.builder[Unit]("ReactRounds")
    .initialState(State(List(e)))
    .renderBackend[ExecutionBackend]
    .build
}

class ExecutionBackend($: BackendScope[Unit, State]) {

  import japgolly.scalajs.react.vdom.html_<^._
  import japgolly.scalajs.react.vdom.{svg_<^ => svg}

  def next = $.modState(_.next)

  //def prev = $.modState(_.prev)

  def render(s: State): VdomElement = {
    <.div(
      ExecutionList(RoundsListProps(s)), // TODO cambiar esto porque al ginal le puse las coor en el State
      <.div(
        <.button(
          ^.onClick --> next,
          "Next"
        ),
        <.button(
          //^.onClick --> prev,
          "Prev"
        ),
      )
    )
  }

}

object ExecutionList {

  import japgolly.scalajs.react.vdom.svg_<^._
  import japgolly.scalajs.react.vdom.{html_<^ => html}

  case class RoundsListProps(s: State)

  val component = ScalaComponent.builder[RoundsListProps]("RoundsList")
    .render_P { props =>
      //val rounds = props.s.rounds.zipWithIndex.toTagMod {case (round,i) => ReactRound(round,i,props.coor) }
      val rounds = props.s.executions.zipWithIndex.toVdomArray { case (execution, _) => ReactExecution.ExecutionComponent(execution) }
      html.<.div(
        rounds
      )
    }.build

  def apply(props: RoundsListProps) = component(props)
}