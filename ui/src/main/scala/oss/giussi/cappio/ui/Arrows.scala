package oss.giussi.cappio.ui

import com.raquo.laminar.api.L._
import oss.giussi.cappio.ui.SampleMain.Point
import oss.giussi.cappio.ui.core.{Crashed, Delivered, Dropped, Indication, Request, Undelivered}

object Arrows {

  // TODO donde va esto?
  def cross(point: Point, size: Int) = {
    val Point(x,y) = point
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

  def dropped(d: Dropped, gridConf: GridConf) = {
    val p1 = gridConf.p(d.sent,d.from)
    val p2 = gridConf.p(d.dropped,d.to)
    arrow(p1,p2,gridConf,Markers.ArrowHeadX)
  }

  def indication(ind: Indication, gridConf: GridConf) = {
    val p1 = gridConf.p(ind.index,ind.process)
    val p2 = Point(p1.x + 10, p1.y - 10) // FIXME
    arrow(p1,p2,gridConf)
  }

  // FIXME request at index 0 will not appear. Alternatives: make vertix at half of the round or start timelines at (n,0) instead of (0,0)
  def request(req: Request, gridConf: GridConf) = {
    val p2 = gridConf.p(req.index,req.process)
    val p1 = Point(p2.x - 10, p2.y - 10) // FIXME
    arrow(p1,p2,gridConf)
  }

  def crashed(crashed: Crashed, gridConf: GridConf) = cross(gridConf.p(crashed.index,crashed.process),3) // TODO size

  // TODO arrowHead
  def arrow(p1: Point, p2: Point, grid: GridConf, arrowHead: String = "arrowhead") = {
    val Point(x1,y1) = p1
    //val Point(x,y) = p2
    val Point(x2,y2) = p2
    //val x2 = x - (grid.arrowHeadSize / 2)
    //val y2 = if (y1 > y) y + (grid.arrowHeadSize / 2) else  y - (grid.arrowHeadSize / 2)
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
    val Point(x1,y) = p1
    val Point(x2,_) = p2
    svg.path(
      svg.d := s"M$x1 $y Q ${(x2 + x1) / 2} ${y - grid.roundHeight} $x2 $y",
      svg.stroke := "black",
      svg.fill := "transparent",
      svg.strokeWidth := "2",
      svg.markerEnd := "url(#arrowhead)"
    )
  }

  def undelivered(action: Undelivered, gridConf: GridConf) = {
    val p1 = gridConf.p(action.sent,action.from)
    val py = gridConf.py(action.to)
    val y = if (p1.y > py) p1.y - (gridConf.roundHeight / 2) else p1.y + (gridConf.roundHeight / 2)
    arrow(p1,Point(p1.x + (gridConf.roundWidth / 2).toInt,y.toInt),gridConf)
  }

  def delivered(action: Delivered, gridConf: GridConf) = {
    val p1 = gridConf.p(action.sent,action.from)
    val p2 = gridConf.p(action.received,action.to)
    arrow(p1,p2,gridConf)
  }

}
