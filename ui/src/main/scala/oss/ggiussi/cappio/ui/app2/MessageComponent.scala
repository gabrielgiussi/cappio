package oss.ggiussi.cappio.ui.app2

import japgolly.scalajs.react.{Callback, ScalaComponent}
import japgolly.scalajs.react.vdom.svg_<^._
import japgolly.scalajs.react.vdom.{html_<^ => html}
import oss.ggiussi.cappio.ui.app.MarkerDefs
import oss.ggiussi.cappio.ui.app2.GridComponent.GridConf


object MessageComponent {

  case class MessageComponentProps(payload: String, instance: String, from: Process, to: Process, sent: Step, delivered: Set[Step], dropped: Set[Step], conf: GridConf)

  case class Arrow(from: Int, to: Int, sent: Int, status: MessageStatus)

  case class ArrowProps(arrow: Arrow, conf: GridConf)

  sealed trait MessageStatus {
    val step: Step
  }

  case class Dropped(step: Step) extends MessageStatus

  case class Delivered(step: Step) extends MessageStatus

  val SelfArrowLine = ScalaComponent.builder[ArrowProps]("SelfArrowLine")
    .render_P {
      case ArrowProps(Arrow(from, to, sent, status), conf) =>
        val (x1, y) = conf.point(sent, from)
        val (x2, _) = conf.point(status.step, to)
        <.path(
          ^.d := s"M$x1 $y Q ${(x2 + x1) / 2} ${y - conf.processHeight} $x2 $y",
          ^.stroke := "black",
          ^.fill := "transparent",
          ^.strokeWidth := "2",
          ^.strokeDasharray := "5,5",
          ^.markerEnd := s"url(#${MarkerDefs.BlackArrowHeadId})"
        )
    }.build

  val ArrowLine = ScalaComponent.builder[ArrowProps]("ArrowLine")
    .render_P { case ArrowProps(Arrow(from, to, sent, status), conf) =>
      val (x1, y1) = conf.point(sent, from)
      val (x2, y2) = conf.point(status.step, to)
      <.line(
        ^.x1 := x1,
        ^.x2 := x2,
        ^.y1 := y1,
        ^.y2 := y2,
        ^.strokeWidth := "2", // TODO this values should come from props
        ^.stroke := (status match {
          case Delivered(_) => "black"
          case Dropped(_) => "red"
        }),
        ^.strokeDasharray := "5,5",
        ^.markerEnd := (status match {
          case Delivered(_) => s"url(#${MarkerDefs.BlackArrowHeadId})"
          case Dropped(_) => s"url(#${MarkerDefs.ReadArrowHeadId})"
        })
      )
    }.build


  val Component = ScalaComponent.builder[MessageComponentProps]("MessageComponent")
    .render_P { case MessageComponentProps(payload, instance, from, to, sent, delivered, dropped, conf) =>
      val arrows = dropped.map(s => Arrow(from, to, sent, Dropped(s))) ++ delivered.map(s => Arrow(from, to, sent, Delivered(s)))
      <.svg(
        arrows.toVdomArray(a =>
          <.svg(
            html.^.key := a.status.step,
            if (from == to) SelfArrowLine(ArrowProps(a,conf)) else ArrowLine(ArrowProps(a, conf)),
            html.^.onClick --> Callback {
              println(s"$payload - $instance")
            } // TODO add delay + show tooltip?
          ))
      )
    }
    .build

}
