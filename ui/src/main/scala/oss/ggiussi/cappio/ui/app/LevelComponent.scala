package oss.ggiussi.cappio.ui.app

import japgolly.scalajs.react.component.Scala.BackendScope
import japgolly.scalajs.react.vdom.HtmlStyles
import japgolly.scalajs.react.{Callback, ScalaComponent}
import oss.ggiussi.cappio.ProcessID
import oss.ggiussi.cappio.core.LinkProtocol.{Deliver, NetworkAction, Send}
import oss.ggiussi.cappio.core.{Action, _}
import oss.ggiussi.cappio.impl.processes.ProcessProtocol.Crash
import oss.ggiussi.cappio.ui.app.LevelBackend.State
import oss.ggiussi.cappio.ui.app2.GridComponent
import oss.ggiussi.cappio.ui.app2.GridComponent.{GridConf, GridProps2}
import oss.ggiussi.cappio.ui.app2.MessageComponent.MessageComponentProps
import oss.ggiussi.cappio.ui.levels.LevelAndSelection


object LevelBackend {

  case class State[S](result: LevelResult[S], selection: List[ActionSelectionProps]) {
    def next(action: Action): State[S] = result match {
      case Success(_) => this
      case Failed(_) => this
      case Pending(level) => copy(level.next(action)) // TODO this may fail! (the action could not be enabled)
    }

    def prev(): State[S] = copy(Pending(result.level.prev()))
  }

  def apply[S](level: LevelAndSelection[S]) = ScalaComponent.builder[Unit]("ReactRounds")
    .initialState(State(Pending(level.level),level.selection))
    .renderBackend[LevelBackend[S]]
    .build
}

class LevelBackend[S]($: BackendScope[Unit, State[S]]) {

  import japgolly.scalajs.react.vdom.html_<^._

  def next(a: Action): Callback = $.modState(_.next(a))

  def prev = $.modState(_.prev)

  val gridConf = GridConf(50, 50, 40, 6, 30)

  def render(s: State[S]): VdomElement = {
    val level = s.result.level
    val processes = level.sched().foldLeft[Map[ProcessID,Option[Int]]](level.processes.map(_ -> None).toMap){
      case (ps,(Crash(id,_),step)) => ps + (id -> Some(step))
      case (ps,_) => ps
    }
    val messages = level.sched().collect {
      case (s, step) if s.isInstanceOf[NetworkAction] => (s.asInstanceOf[NetworkAction], step)
    }.groupBy(_._1.id).mapValues { acts =>
      val (send,sent) = acts.find(_._1.isInstanceOf[Send]).map(s => (s._1.asInstanceOf[Send],s._2)).get // TODO
      val delivers = acts.collect {
        case (Deliver(_,_),step) => step
      }
      println(s"From ${send.header.from} => ${send.header.to}")
      MessageComponentProps(send.msg.payload.toString,send.header.instance.id,send.header.from,send.header.to,sent,delivers,Set(),gridConf)
    }.values.toList

    def c1() = {
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
              ExecutionComponent(ExecutionProps(s.selection, next, exec, (index == level.executions.length - 1) && !s.result.ended,level.nextStep)),
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
      level.state.map(st => ConditionsComponent(s.result.level.conditions.map(c => (c.description,c(st))))),
      GridComponent.Component(GridProps2(processes.toSet,messages,gridConf))
    )
  }
}