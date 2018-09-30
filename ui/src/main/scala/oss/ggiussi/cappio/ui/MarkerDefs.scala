package oss.ggiussi.cappio.ui

import japgolly.scalajs.react.vdom.Implicits.vdomAttrVtJsAny
import japgolly.scalajs.react.vdom.svg_<^._

object Constants {
  val arrowHeadSize = 6

  val roundW: Double = 40

  val nodeH: Double = 20

  val linkH: Double = 10

  val spaceBetweenNodes = 50

  val point = (step: Int,process: Int) => (step * roundW, process * spaceBetweenNodes)

}

object MarkerDefs {

  import Constants._

  private val arrowHead = <.marker(
    ^.id := "arrowhead",
    ^.markerWidth := arrowHeadSize,
    ^.markerHeight := arrowHeadSize,
    ^.markerUnits := "userSpaceOnUse",
    VdomAttr("refX") := "0",
    VdomAttr("refY") := arrowHeadSize / 2,
    VdomAttr("orient") := "auto",
    <.path(
      ^.d := s"M0,0 L0,${arrowHeadSize} L${arrowHeadSize},${arrowHeadSize / 2} z",
      ^.fill := "black"
    )
  )

  val markers = List(arrowHead).toTagMod

  def arrow(coor: (Double, Double)) = {
    val y1 = coor._1.toInt
    val y2 = if (y1 > coor._2) (coor._2 + arrowHeadSize).toInt else (coor._2 - arrowHeadSize).toInt
    <.line(
      (^.x1 := roundW / 2)(vdomAttrVtJsAny),
      (^.x2 := roundW / 2)(vdomAttrVtJsAny),
      (^.y1 := y1)(vdomAttrVtJsAny),
      (^.y2 := y2)(vdomAttrVtJsAny),
      ^.stroke := "black",
      ^.strokeWidth := "2",
      ^.markerEnd := "url(#arrowhead)"
    )
  }

}
