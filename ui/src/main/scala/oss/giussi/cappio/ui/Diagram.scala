package oss.giussi.cappio.ui

import oss.giussi.cappio.Processes
import com.raquo.laminar.api.L.{svg => s, _}
import oss.giussi.cappio.ui.core.{Action, Crashed, Delivered, Dropped, Indication, PendingRead, PendingWrite, ReadReturned, Request, Undelivered, WriteReturned}

object Diagram {

  def apply(processes: Processes, $actions: Signal[List[Action]]) = {
    val labelWidth = 20 // TODO must fit the greatest process id number or use autoscale
    val $bus = new EventBus[GridConfImpl]
    val gridConf = GridConfImpl(40, processes)
    val $gridConf = $bus.events.toSignal(gridConf)
    val height = (processes.ids.size * gridConf.roundHeight) + gridConf.roundHeight // padding
    val width = "10000" // TODO scrolled
    div(
      styleAttr := "overflow: scroll; overflow-y: hidden;",
      s.svg(
        s.height := height.toString,
        s.width := width,
        child <-- $gridConf.map(c => Markers.defs(c.arrowHeadSize, c.crossSize)),
        labels(gridConf, labelWidth, height),
        timelines(gridConf, $gridConf, labelWidth + 1, $actions),
        grid(gridConf,labelWidth + 1, height)
      ),
      input(`type` := "number", inContext(thisNode => onChange.mapTo(thisNode.ref.value).map(w => gridConf.copy(roundWidth = w.toInt)) --> $bus))
    )
  }

  def renderAction($gridConf: Signal[GridConf])(id: String, initial: Action, signal: Signal[Action]) = {
    s.svg(
      child <-- signal.combineWith($gridConf).map {
        case (d: Delivered,gridConf) => Arrows.delivered(d,gridConf)
        case (u: Undelivered, gridConf) => Arrows.undelivered(u,gridConf)
        case (c: Crashed,gridConf) => Arrows.crashed(c,gridConf)
        case (r: Request,gridConf) => Arrows.request(r,gridConf)
        case (i: Indication,gridConf) => Arrows.indication(i,gridConf)
        case (d: Dropped,gridConf) => Arrows.dropped(d,gridConf)
        case (r: PendingRead,gridConf) => Arrows.pendingRead(r,gridConf)
        case (r: ReadReturned,gridConf) => Arrows.readReturned(r,gridConf)
        case (r: PendingWrite,gridConf) => Arrows.pendingWrite(r,gridConf)
        case (r: WriteReturned,gridConf) => Arrows.writeReturned(r,gridConf)
      }
    )
  }

  def grid(gridConf: GridConf, x: Int, height: Double) = {
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

  def labels(gridConf: GridConf, width: Int, heigth: Double) = {
    s.svg(
      s.x := "0",
      s.y := "0",
      s.width := width.toString,
      s.height := heigth.toString,
      gridConf.processes.map(p =>
        s.text(
          s.x := "0",
          s.y := gridConf.y(p).toString,
          s"${p.id}"
        )
      )
    )
  }



  def timelines(gridConf: GridConf, $gridConf: Signal[GridConf], x: Int, $actions: Signal[List[Action]]) = {
    s.svg(
      s.x := x.toString,
      s.y := "0",
      //s.viewBox <-- $current.filter(_ > 0).map(x => s"$x 0 $width $height"), // Lo resolvi con el scrol del div por ahora.
      gridConf.processes.map { p =>
        val py = gridConf.y(p).toString
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
      children <-- $actions.splitIntoSignals(_.id)(renderAction($gridConf))
    )
  }

}
