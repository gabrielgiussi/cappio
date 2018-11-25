package oss.ggiussi.cappio.ui.app2

import japgolly.scalajs.react.{Callback, ScalaComponent}
import japgolly.scalajs.react.vdom.svg_<^._
import japgolly.scalajs.react.vdom.{html_<^ => html}
import oss.ggiussi.cappio.ui.app.Grid.{ProcessTimeline, ProcessTimelineProps}
import oss.ggiussi.cappio.ui.app.MarkerDefs
import oss.ggiussi.cappio.ui.app2.MessageComponent.MessageComponentProps

object GridComponent {

  case class GridConf(rounds: Int, roundWidth: Double, processHeight: Double, arrowHeadSize: Double, labelWidth: Double) {
    def point(step: Int, process: Int): (Double, Double) = ((step * roundWidth) + labelWidth, process * processHeight)
  }

  case class GridProps(processes: Set[(Process, Option[Step])], conf: GridConf)

  val ProcessTimeline = ScalaComponent.builder[(Process,Option[Step],GridConf)]("ProcessTimeline")
    .render_P { case (p,crashed,conf) =>
      val (x0,y) = conf.point(0,p)
      val (xN,_) = conf.point(conf.rounds,p)
      val label = <.text(
        ^.x := 0,
        ^.y := y,
        VdomAttr("lengthAdjust") := "spacingAndGlyphs",
        VdomAttr("textLength") := conf.labelWidth,
        s"Process $p"
      )
      crashed match {
        case None =>
          <.svg(
            label,
            <.line(
              ^.x1 := x0,
              ^.x2 := xN,
              ^.y1 := y,
              ^.y2 := y,
              ^.strokeWidth := "2",
              ^.stroke := "black",
            )
          )
        case Some(step) =>
          val crashedX = (step * conf.roundWidth).toInt
          <.svg(
            label,
            <.line(
              ^.x1 := x0,
              ^.x2 := crashedX,
              ^.y1 := y,
              ^.y2 := y,
              ^.strokeWidth := "2",
              ^.stroke := "black",
            ),
            <.line(
              ^.x1 := crashedX,
              ^.x2 := xN,
              ^.y1 := y,
              ^.y2 := y,
              ^.strokeWidth := "2",
              ^.stroke := "red",
            ),
            CrossComponent((crashedX, y.toInt, 5))
          )
      }
    }.build

  val Processes = ScalaComponent.builder[(Set[(Process, Option[Step])], GridConf)]("ProcessList")
    .render_P { case (processes, conf) =>
      processes.toList.sortBy(_._1).toVdomArray { case (p, crashed) =>
        <.svg(
          html.^.key := p,
          //^.y := (p * conf.processHeight),
          ProcessTimeline((p,crashed,conf))
        )
      }
    }.build

  val CrossComponent = ScalaComponent.builder[(Int,Int,Int)]("Cross")
    .render_P { case (x,y,size) =>
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

  val Component = ScalaComponent.builder[GridProps]("Grid")
    .render_P { case GridProps(processes, conf) =>
      <.svg(
        <.defs(
          MarkerDefs.BlackArrowHead(conf.arrowHeadSize),
          MarkerDefs.ReadArrowHead(conf.arrowHeadSize)
        ),
        ^.width := 10000,
        ^.height := 10000,
        ^.x := 0,
        ^.y := 0,
        Processes((processes, conf)),
        <.svg(
          ^.x := 0,
          ^.y := conf.processHeight,
          MessageComponent.Component(MessageComponentProps("prueba", "beb-bcast", 0, 1, 0, Set(1), Set(), conf)), // TODO
          MessageComponent.Component(MessageComponentProps("prueba", "beb-bcast", 1, 1, 0, Set(), Set(1), conf))
        )
      )
    }.build


}
