package oss.giussi.cappio.ui

import com.raquo.laminar.api.L._
import com.raquo.laminar.nodes.ReactiveSvgElement
import org.scalajs.dom.svg.Line
import oss.giussi.cappio.ui.core._


/*
Solucionar
todo lo referido a tamaños que esten hardcodeados
parametros q reciben las arrows

grouped actions
ver si puedo tipar el payload de las actions
 */

object Arrows {

  val actionSelected = new EventBus[Action]
  actionSelected.events.addObserver(Observer.apply(println))(unsafeWindowOwner) // FIXME remove

  // TODO donde va esto?
  def cross(point: Point, size: Double) = {
    val Point(x, y) = point
    svg.svg(
      svg.line(
        svg.x1 := (x - size).toString,
        svg.y1 := (y - size).toString,
        svg.x2 := (x + size).toString,
        svg.y2 := (y + size).toString,
        svg.strokeWidth := "2",
        svg.stroke := "red"
      ),
      svg.line(
        svg.x1 := (x - size).toString,
        svg.y1 := (y + size).toString,
        svg.x2 := (x + size).toString,
        svg.y2 := (y - size).toString,
        svg.strokeWidth := "2",
        svg.stroke := "red"
      )
    )
  }

  sealed trait Orientation
  case object Up extends Orientation
  case object Down extends Orientation

  def indication(ind: Indication, gridConf: GridConf) = {
    val p = gridConf.point(ind.index,ind.process)
    val y = p.y - (gridConf.roundHeight / 3) // issue 79
    arrow(p, Point(p.x + (gridConf.roundWidth / 6), y), Markers.ArrowHeadEmpty)
  }

  // FIXME request at index 0 will not appear. Alternatives: make vertix at half of the round or start timelines at (n,0) instead of (0,0)
  def request(req: Request, gridConf: GridConf) = {
    val p2 = gridConf.point(req.index, req.process)
    val p1 = Point(p2.x - (gridConf.roundWidth / 2), p2.y - (gridConf.roundWidth / 2))
    arrow(p1, p2)
  }

  def crashed(crashed: Crashed, gridConf: GridConf) = cross(gridConf.point(crashed.index, crashed.process), gridConf.crossSize)

  // TODO arrowHead
  def arrow(p1: Point, p2: Point, arrowHead: String = "arrowhead") = {
    val Point(x1, y1) = p1
    //val Point(x,y) = p2
    val Point(x2, y2) = p2
    //val x2 = x - (grid.arrowHeadSize / 2)
    //val y2 = if (y1 > y) y + (grid.arrowHeadSize / 2) else  y - (grid.arrowHeadSize / 2) FIXME que la linea se acorte y no se pise con el marker-end

    svg.line(
      svg.x1 := x1.toString,
      svg.x2 := x2.toString,
      svg.y1 := y1.toString,
      svg.y2 := y2.toString,
      svg.stroke := "black",
      svg.strokeWidth := "2",
      svg.markerEnd := s"url(#${arrowHead})"
    )
  }

  // FIXME duplicated with arrow
  def arrow2(p1: Point, p2: Point) = {
    val Point(x1, y1) = p1
    val Point(x2, y2) = p2
    svg.line(
      svg.x1 := x1.toString,
      svg.x2 := x2.toString,
      svg.y1 := y1.toString,
      svg.y2 := y2.toString,
      svg.stroke := "black",
      svg.strokeWidth := "2"
    )
  }

  def selfArrow(p1: Point, p2: Point, grid: GridConf) = {
    val Point(x1, y) = p1
    val Point(x2, _) = p2
    svg.path(
      svg.d := s"M$x1 $y Q ${(x2 + x1) / 2} ${y - grid.roundHeight} $x2 $y",
      svg.stroke := "black",
      svg.fill := "transparent",
      svg.strokeWidth := "2",
      svg.markerEnd := "url(#arrowhead)"
    )
  }

  private def shortArrow(p: Point, orientation: Orientation, gridConf: GridConf, arrowHead: String) = {
    val y = orientation match {
      case Up => p.y - (gridConf.roundHeight / 3)
      case Down => p.y + (gridConf.roundHeight / 3)
    }
    arrow(p, Point(p.x + (gridConf.roundWidth / 2), y), arrowHead)
  }

  private def shortArrowNetwork(action: NetworkAction, gridConf: GridConf, arrowHead: String) = {
    val p = gridConf.point(action.sent, action.from)
    val py = gridConf.y(action.to)
    val orientation = if (p.y > py) Up else Down
    shortArrow(p, orientation, gridConf, arrowHead)
  }

  def undelivered(action: Undelivered, gridConf: GridConf) = shortArrowNetwork(action,gridConf,Markers.ArrowHead)

  def dropped(action: Dropped, gridConf: GridConf) = shortArrowNetwork(action,gridConf,Markers.ArrowHeadX)


  /*
  private def rdwrReturned(start: Index, returned: Index, process: ProcessId, gridConf: GridConf) = svg.svg(
    circle(gridConf.point(start,process),gridConf.pointSize),
    arrow2(gridConf.point(start,process),gridConf.point(returned,process)),
    circle(gridConf.point(returned,process),gridConf.pointSize)
  )
  def pendingRead(action: PendingRead, gridConf: GridConf) = circle(gridConf.point(action.start,action.process),gridConf.pointSize)

  def readReturned(action: ReadReturned, gridConf: GridConf) = rdwrReturned(action.start,action.returned,action.process,gridConf)

  def pendingWrite(action: PendingWrite, gridConf: GridConf) = circle(gridConf.point(action.start,action.process),gridConf.pointSize)

  // FIXME duplicated code with readReturned
  def writeReturned(action: WriteReturned, gridConf: GridConf) = rdwrReturned(action.start,action.returned,action.process,gridConf)
   */

  def delivered(action: Delivered, gridConf: GridConf) = {
    val p1 = gridConf.point(action.sent, action.from)
    val p2 = gridConf.point(action.received, action.to)
    def selfDelivered = {
      svg.path(
        svg.d := s"M${p1.x},${p1.y} Q${(p2.x + p1.x) / 2} ${p1.y - gridConf.roundHeight} ${p2.x},${p2.y}",
        svg.fill := "transparent",
        svg.markerEnd := "url(#arrowhead)",
        svg.stroke := "black",
        svg.strokeWidth := "2"
      )
    }
    if (action.from == action.to) selfDelivered else arrow(p1, p2)
  }



  def circle(p: Point, size: Double) = {
    svg.circle(
      svg.cx := p.x.toString,
      svg.cy := p.y.toString,
      svg.r := size.toString,
      svg.fill := "black"
    )
  }

}
