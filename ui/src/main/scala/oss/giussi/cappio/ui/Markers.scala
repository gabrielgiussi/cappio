package oss.giussi.cappio.ui

import oss.giussi.cappio.ui.SampleMain.Point
import com.raquo.laminar.api.L._
import com.raquo.laminar.nodes.{ReactiveChildNode, ReactiveHtmlElement}
import com.raquo.laminar.api.L.svg

object Markers {

  val ArrowHead = "arrowhead"

  val ArrowHeadX = "arrowheadx"

  def defs(arrowHeadSize: Int) = svg.defs(
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
      svg.markerWidth := "100", // TODO
      svg.markerHeight := "100",
      svg.markerUnits := "userSpaceOnUse",
      svg.orient := "auto",
      svg.refX := arrowHeadSize.toString,
      svg.refY := (arrowHeadSize / 2).toString,
      svg.path(
        svg.d := s"M0,0 L0,${arrowHeadSize} L${arrowHeadSize},${arrowHeadSize / 2} z",
        svg.fill := "black"
      ),
      Arrows.cross(Point(arrowHeadSize,arrowHeadSize / 2),3) // TODO or another path?
    )
  )

}
