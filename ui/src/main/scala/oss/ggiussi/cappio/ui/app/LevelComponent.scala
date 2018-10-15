package oss.ggiussi.cappio.ui.app

import japgolly.scalajs.react.component.Scala.BackendScope
import japgolly.scalajs.react.vdom.HtmlStyles
import japgolly.scalajs.react.{Callback, ScalaComponent}
import oss.ggiussi.cappio.core.{Action, _}
import oss.ggiussi.cappio.ui.app.Grid.GridProps
import oss.ggiussi.cappio.ui.app.LevelBackend.State
import oss.ggiussi.cappio.ui.levels.Level1


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
    println(s.result.level.state().get.asInstanceOf[Level1.State]._3)
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
          TagMod(
            ^.cls := "waves-effect waves-light btn"
          ),
          ^.disabled := (level.executions.size == 1),
          ^.onClick --> prev,
          "Previous"
        )
      )
    }

    <.div(
      <.div(
        ^.cls := "d-flex justify-content-between flex-wrap flex-md-nowrap align-items-center pt-3 pb-2 mb-3 border-bottom",
        <.h1("Level 1")
      ),
      c1(),
      <.div(
        <.label(s.result.level.executions.last.sched().mkString(" , "))
      ),
      s.result.level.state.map(st => ConditionsComponent(s.result.level.conditions.map(c => (c.description,c(st))))),
      Grid.Grid(GridProps(3, s.result.level.executions))
    )
  }
}