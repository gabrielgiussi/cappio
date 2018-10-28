package oss.ggiussi.cappio.ui.app

import japgolly.scalajs.react.{Callback, ScalaComponent}
import oss.ggiussi.cappio.core.LinkProtocol.{Deliver, Drop, Send}
import oss.ggiussi.cappio.core.{Action, Execution}
import oss.ggiussi.cappio.impl.links.{Message, MessageID}
import Constants._
import oss.ggiussi.cappio.ProcessID
import oss.ggiussi.cappio.impl.processes.ProcessProtocol.Crash

// https://www.sarasoueidan.com/blog/mimic-relative-positioning-in-svg/
// http://tutorials.jenkov.com/svg/svg-viewport-view-box.html
object Grid {

  import japgolly.scalajs.react.vdom.svg_<^._
  import japgolly.scalajs.react.vdom.{html_<^ => html}

  case class ProcessTimelineProps(rounds: Int, processes: Int, proccessState: Int => Callback)

  case class GridProps(processes: Int, executions: List[Execution[_]])

  sealed trait MessageC {
    def msgID: MessageID

    def to: Int
  }

  case class GridSend(step: Int, from: Int, to: Int, msg: Message) extends MessageC {
    override def msgID: MessageID = msg.id
  }

  case class GridDeliver(step: Int, from: Int, to: Int, msg: Message) extends MessageC {
    override def msgID: MessageID = msg.id
  }

  case class GridDrop(step: Int, from: Int, to: Int, msgID: MessageID) extends MessageC

  case class Arrow(from: Int, to: Int, sent: Int, delivered: Int, ok: Boolean)

  val SelfArrow = ScalaComponent.builder[Arrow]("SelfArrow")
    .render_P { case Arrow(self, _, sent, delivered, ok) =>
      val (x1, y1) = point(sent, self)
      val (x2, y2) = point(delivered, self)
      <.path(
        ^.d := s"M$x1 $y1 C ${x1 + 10} ${y1 + 10}, ${x1 + 10} ${y1 + 10}, $x2 $y2",
        ^.fill := "transparent",
        ^.strokeWidth := "2",
        ^.stroke := {
          if (ok) "black" else "red"
        },
        ^.strokeDasharray := "5,5",
        ^.markerEnd := "url(#arrowhead)"
      )
    }.build

  val ArrowL = ScalaComponent.builder[Arrow]("ArrowLine")
    .render_P { case Arrow(from, to, sent, delivered, ok) =>
      val (x1, y1) = point(sent, from)
      val (x2, y2) = point(delivered, to)
      <.line(
        ^.x1 := x1,
        ^.x2 := x2,
        ^.y1 := y1,
        ^.y2 := (y2 + {
          if (y1 < y2) (arrowHeadSize * -1) else arrowHeadSize
        }),
        ^.strokeWidth := "2",
        ^.stroke := {
          if (ok) "black" else "red"
        },
        ^.strokeDasharray := "5,5",
        ^.markerEnd := "url(#arrowhead)"
      )
    }.build

  val ArrowLine = ScalaComponent.builder[Arrow]("Arrow")
    .render_P {
      case a@Arrow(from, to, _, _, _) if from == to => SelfArrow(a)
      case a => ArrowL(a)
    }.build

  val ProcessTimeline = ScalaComponent.builder[(Int, Callback)]("ProcessTimeline")
    .render_P { case (rounds, c) =>
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

  val CrashComponent = ScalaComponent.builder[(ProcessID, Int,Int)]("Crash")
    .render_P { case (id,step,size) =>
      val (x,y) = point(step,id)
        <.svg(
          <.line(
            ^.x1 := x - size,
            ^.y1 := y - size,
            ^.x2 := x + size,
            ^.y2 := y + size,
            ^.strokeWidth := "2",
            ^.stroke := "red",
          ),
          <.line(
            ^.x1 := x - size,
            ^.y1 := y + size,
            ^.x2 := x + size,
            ^.y2 := y - size,
            ^.strokeWidth := "2",
            ^.stroke := "red",
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

       val messages = actions.collect {
          case (Deliver(f, to, _,msg), index) => (2,GridDeliver(index, f, to, msg))
          case (Send(f, to, _, msg), index) => (1,GridSend(index, f, to, msg))
          case (Drop(id,_), index) => (2,GridDrop(index, id.from, id.to, id))
        }

      val arrows = messages.groupBy(_._2.msgID).values.map(_.sortBy(_._1).map(_._2)).flatMap {
        //case List(GridDeliver(delivered, from, to, _), GridSend(sent, _, _, _)) => Some(Arrow(from, to, sent, delivered, true))
        case List(GridSend(sent, from, to, _), GridDeliver(delivered, _, _, _)) => Some(Arrow(from, to, sent, delivered, true))
        case List(GridSend(sent, from, to, _), GridDrop(delivered, _, _, _)) => Some(Arrow(from, to, sent, delivered, false))
        case _ => None
      }

      val crash = actions.collect {
        case (Crash(id,_),step) => CrashComponent(id,step,5)
      }

      val as = arrows.zipWithIndex.toVdomArray { case (a, index) =>
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
        Processes(ProcessTimelineProps(10, processes, (i: Int) => Callback {
          println(executions.last.state)
        })), // TODO rounds
        as,
        <.svg(
          crash.toVdomArray
        )
      )
    }.build

}
