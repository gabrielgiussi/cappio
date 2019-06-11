package oss.giussi.cappio.ui

import oss.giussi.cappio.Processes
import com.raquo.laminar.api.L.{svg => s, _}
import oss.giussi.cappio.ui.core.{Action, Crashed, Delivered, Dropped, Indication, Request, Undelivered}

object Diagram {

  def apply(processes: Processes, $actions: EventStream[List[Action]]) = {
    val labelWidth = 20 // TODO must fit the greatest process id number or use autoscale
    val gridConf = GridConf(10, 40, 100, 10, processes)
    val height = (processes.ids.size * gridConf.roundHeight).toInt
    val width = "10000" // TODO scrolled
    s.svg(
      s.height := height.toString,
      s.width := width,
      Markers.defs(gridConf.arrowHeadSize.toInt),
      labels(gridConf, labelWidth, height),
      timelines(gridConf, labelWidth + 1, $actions),
      grid(gridConf,labelWidth + 1, height)
    )
  }

  def renderAction(gridConf: GridConf)(id: String, initial: Action, $action: EventStream[Action]) = {
    val signal = $action.toSignal(initial) // TODO no se si esta bien de acuerdo a https://github.com/raquo/Laminar/blob/master/docs/Documentation.md#performant-children-rendering--childrencommand
    s.svg(
      child <-- signal.map {
        case d: Delivered => Arrows.delivered(d,gridConf)
        case u: Undelivered => Arrows.undelivered(u,gridConf)
        case c: Crashed => Arrows.crashed(c,gridConf)
        case r: Request => Arrows.request(r,gridConf)
        case i: Indication => Arrows.indication(i,gridConf)
        case d: Dropped => Arrows.dropped(d,gridConf)
      }
    )
  }

  def grid(gridConf: GridConf, x: Int, height: Int) = {
    s.svg(
      s.x := x.toString,
      s.y := "0",
      // TODO random number 500
      (1 to 500).map(i => s.line(
        s.x1 := (i * gridConf.roundWidth).toString,
        s.x2 := (i * gridConf.roundWidth).toString,
        s.y1 := "0",
        s.y2 := height.toString,
        s.strokeDashArray := "5,5",
        s.stroke := "black",
        s.opacity := "0.5"
      ))
    )
  }

  def labels(gridConf: GridConf, width: Int, heigth: Int) = {
    s.svg(
      s.x := "0",
      s.y := "0",
      s.width := width.toString,
      s.height := heigth.toString,
      gridConf.order.map(p =>
        s.text(
          s.x := "0",
          s.y := gridConf.py(p._1).toString,
          s"${p._1.id}"
        )
      )
    )
  }



  def timelines(gridConf: GridConf, x: Int, $actions: EventStream[List[Action]]) = {
    s.svg(
      s.x := x.toString,
      s.y := "0",
      //s.viewBox <-- $current.filter(_ > 0).map(x => s"$x 0 $width $height"), // Lo resolvi con el scrol del div por ahora.
      gridConf.processes.ids.toList.map { p =>
        val py = gridConf.py(p).toString
        s.line(
          s.stroke := "black",
          s.strokeWidth := "3",
          s.x1 := "0",
          s.x2 := "1000000", // TODO rounds
          s.y1 := py,
          s.y2 := py,
          s.r := "30",
        )
      },
      children <-- $actions.split(_.id)(renderAction(gridConf))
    )
  }

}
