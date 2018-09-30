package oss.ggiussi.cappio.ui

import japgolly.scalajs.react.{Callback, ScalaComponent}
import oss.ggiussi.cappio.core.LinkProtocol._
import oss.ggiussi.cappio.core.{Action, Execution}
import oss.ggiussi.cappio.impl.links.Message
import oss.ggiussi.cappio.ui.Constants._

// https://www.sarasoueidan.com/blog/mimic-relative-positioning-in-svg/
// http://tutorials.jenkov.com/svg/svg-viewport-view-box.html
object Grid {

  import japgolly.scalajs.react.vdom.svg_<^._
  import japgolly.scalajs.react.vdom.{html_<^ => html}

  case class ProcessTimelineProps(rounds: Int, processes: Int, proccessState: Int => Callback)

  case class GridProps(processes: Int, executions: List[Execution[_]])

  sealed trait MessageC {
    def msg: Message

    def to: Int
  }

  case class GridSend(step: Int, from: Int, to: Int, msg: Message) extends MessageC

  case class GridDeliver(step: Int, from: Int, to: Int, msg: Message) extends MessageC

  case class Arrow(from: Int, to: Int, sent: Int, delivered: Int)

  val ArrowLine = ScalaComponent.builder[Arrow]("ArrowLine")
    .render_P { case Arrow(from, to, sent, delivered) =>
      val (x1, y1) = point(sent, from)
      val (x2, y2) = point(delivered, to)
      <.line(
        ^.x1 := x1,
        ^.x2 := x2,
        ^.y1 := y1,
        ^.y2 := (y2 + { if (y1 < y2) (arrowHeadSize * -1) else arrowHeadSize }),
        ^.strokeWidth := "2",
        ^.stroke := "black",
        ^.strokeDasharray := "5,5",
        ^.markerEnd := "url(#arrowhead)"
      )
    }.build

  val ProcessTimeline = ScalaComponent.builder[(Int,Callback)]("Execution")
    .render_P { case (rounds,c) =>
      <.svg(
        <.line(
          html.^.onClick --> c,
          ^.x := "0",
          ^.x2 := rounds * roundW,
          ^.y1 := "0",
          ^.y2 := "0",
          ^.strokeWidth := "2",
          ^.stroke := "black",
        )
      )
    }.build

  val Processes = ScalaComponent.builder[ProcessTimelineProps]("Execution")
    .render_P { case ProcessTimelineProps(rounds, processes, callback) =>
      (0 to processes - 1).toVdomArray(p =>
        <.svg(
          html.^.key := p,
          ^.y := (p * spaceBetweenNodes),
          ProcessTimeline((rounds, callback(p)))
        )
      )
    }.build

  val Grid = ScalaComponent.builder[GridProps]("Grid")
    .render_P { case GridProps(processes, executions) =>

      val actions = executions.zipWithIndex.foldLeft(List.empty[(Action, Int)]) { (acc, a) => acc ++ (a._1.sched().toSet -- acc.map(_._1).toSet).map(_ -> a._2) }
        .collect {
          case (Deliver(f, to, msg), index) => GridDeliver(index, f, to, msg)
          case (Send(f, to, msg), index) => GridSend(index, f, to, msg)
        }

      val arrows = actions.groupBy(_.msg.id).values.flatMap {
        case List(GridDeliver(delivered, from, to, _), GridSend(sent, _, _, _)) => Some(Arrow(from, to, sent, delivered))
        case List(GridSend(sent, from, to, _), GridDeliver(delivered, _, _, _)) => Some(Arrow(from, to, sent, delivered))
        case _ => None
      }

      val as = arrows.zipWithIndex.toVdomArray { case (a,index) =>
        <.svg(
          html.^.key := index,
          ArrowLine(a)
        )
      }

      <.svg(
        <.defs(
          MarkerDefs.markers
        ),
        ^.width := 10000,
        ^.height := 10000,
        ^.x := 0,
        ^.y := 0,
        Processes(ProcessTimelineProps(10, processes,(i: Int) => Callback {
          println(executions.last.state)
        })), // TODO rounds
        as
      )
    }.build

}
