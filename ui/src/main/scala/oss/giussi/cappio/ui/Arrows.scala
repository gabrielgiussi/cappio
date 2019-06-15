package oss.giussi.cappio.ui

import com.raquo.laminar.api.L._
import oss.giussi.cappio.{Network, ProcessId}
import oss.giussi.cappio.ui.core.{Crashed, Delivered, Dropped, Indication, NetworkAction, Request, Undelivered}


/*
Solucionar
todo lo referido a tamaños que esten hardcodeados
parametros q reciben las arrows

grouped actions
ver si puedo tipar el payload de las actions
 */

object Arrows {

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
    shortArrow(gridConf.point(ind.index,ind.process),Up,gridConf,Markers.ArrowHead)
  }

  // FIXME request at index 0 will not appear. Alternatives: make vertix at half of the round or start timelines at (n,0) instead of (0,0)
  def request(req: Request, gridConf: GridConf) = {
    val p2 = gridConf.point(req.index, req.process)
    val p1 = Point(p2.x - (gridConf.roundWidth / 2), p2.y - (gridConf.roundWidth / 2))
    arrow(p1, p2, gridConf)
  }

  def crashed(crashed: Crashed, gridConf: GridConf) = cross(gridConf.point(crashed.index, crashed.process), gridConf.crossSize)

  // TODO arrowHead
  def arrow(p1: Point, p2: Point, grid: GridConf, arrowHead: String = "arrowhead") = {
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
      case Up => p.y - (gridConf.roundHeight / 2)
      case Down => p.y + (gridConf.roundHeight / 2)
    }
    arrow(p, Point(p.x + (gridConf.roundWidth / 2), y), gridConf, arrowHead)
  }

  private def shortArrowNetwork(action: NetworkAction, gridConf: GridConf, arrowHead: String) = {
    val p = gridConf.point(action.sent, action.from)
    val py = gridConf.y(action.to)
    val orientation = if (p.y > py) Up else Down
    shortArrow(p, orientation, gridConf, arrowHead)
  }

  def undelivered(action: Undelivered, gridConf: GridConf) = shortArrowNetwork(action,gridConf,Markers.ArrowHead)

  def dropped(action: Dropped, gridConf: GridConf) = shortArrowNetwork(action,gridConf,Markers.ArrowHeadX)

  def delivered(action: Delivered, gridConf: GridConf) = {
    val p1 = gridConf.point(action.sent, action.from)
    val p2 = gridConf.point(action.received, action.to)
    arrow(p1, p2, gridConf)
  }

}
