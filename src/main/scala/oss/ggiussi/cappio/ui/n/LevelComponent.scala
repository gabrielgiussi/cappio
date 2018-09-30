package oss.ggiussi.cappio.ui.n

import japgolly.scalajs.react.{Callback, ScalaComponent}
import japgolly.scalajs.react.component.Scala.BackendScope
import japgolly.scalajs.react.vdom.HtmlStyles
import oss.ggiussi.cappio.core.Action
import oss.ggiussi.cappio.core._
import japgolly.scalajs.react.vdom.{svg_<^ => svg}
import oss.ggiussi.cappio.ui.Grid
import oss.ggiussi.cappio.ui.Grid.GridProps
import oss.ggiussi.cappio.ui.n.LevelBackend.State


object LevelBackend {

  case class State[S](result: LevelResult[S]) {
    def next(action: Action): State[S] = result match {
      case Success(_) => this
      case Failed(_) => this
      case Pending(level) => State(level.next(action))
    }

    def prev(): State[S] = State(Pending(result.level.prev()))
  }

  def apply[S](level: Level[S]) = ScalaComponent.builder[Unit]("ReactRounds")
    .initialState(State(Pending(level)))
    .renderBackend[LevelBackend[S]]
    .build
}

class LevelBackend[S]($: BackendScope[Unit, State[S]]) {

  import japgolly.scalajs.react.vdom.html_<^._

  def next(a: Action): Callback = $.modState(_.next(a))

  def prev = $.modState(_.prev)


  def render(s: State[S]): VdomElement = {
    def c1() = {
      val level = s.result.level
      val r = s.result match {
        case Success(_) => Some("Success")
        case Failed(_) => Some("Failed")
        case _ => None
      }
      <.div(
        r.map(s => <.label(s)),
        <.ul(
          HtmlStyles.listStyleType.none,
          level.executions.zipWithIndex.last match { case (exec, index) =>
            <.li(
              HtmlStyles.display.`inline-block`,
              ExecutionComponent(ExecutionProps(next, exec, (index == level.executions.length - 1) && !s.result.ended,level.nextStep)),
              ^.key := index
            )
          }
        ),
        <.button(
          ^.disabled := (level.executions.size == 1),
          ^.onClick --> prev,
          "Previous"
        )
      )
    }

    <.div(
      c1(),
      <.div(
        <.label(s.result.level.executions.last.sched().mkString(" , "))
      ),
      Grid.Grid(GridProps(3, s.result.level.executions))
    )
  }
}