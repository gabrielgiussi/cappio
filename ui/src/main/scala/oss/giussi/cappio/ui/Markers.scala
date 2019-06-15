package oss.giussi.cappio.ui

import com.raquo.laminar.api.L.svg

object Markers {

  val ArrowHead = "arrowhead"

  val ArrowHeadX = "arrowheadx"

  def defs(arrowHeadSize: Double, crossSize: Double) = svg.defs(
    svg.marker(
      svg.id := ArrowHead,
      svg.markerWidth := arrowHeadSize.toString,
      svg.markerHeight := arrowHeadSize.toString,
      svg.markerUnits := "userSpaceOnUse",
      svg.orient := "auto",
      svg.refX := arrowHeadSize.toString, // TODO
      svg.refY := (arrowHeadSize / 2).toString,
      svg.path(
        svg.d := s"M0,0 L0,${arrowHeadSize} L${arrowHeadSize},${arrowHeadSize / 2} z",
        svg.fill := "black"
      )
    ),
    svg.marker(
      svg.id := ArrowHeadX,
      svg.markerWidth := "100000", // TODO
      svg.markerHeight := "100000",
      svg.markerUnits := "userSpaceOnUse",
      svg.orient := "auto",
      svg.refX := arrowHeadSize.toString,
      svg.refY := (arrowHeadSize / 2).toString,
      svg.path(
        svg.d := s"M0,0 L0,${arrowHeadSize} L${arrowHeadSize},${arrowHeadSize / 2} z",
        svg.fill := "black"
      ),
      // TODO or another path?
      Arrows.cross(Point(arrowHeadSize,arrowHeadSize / 2), crossSize) // FIXME se ve cortada la X

    )
  )

}
