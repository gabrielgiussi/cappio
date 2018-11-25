package oss.ggiussi.cappio.ui.app

import japgolly.scalajs.react.vdom.Implicits.vdomAttrVtJsAny
import japgolly.scalajs.react.vdom.svg_<^._
import japgolly.scalajs.react.ScalaComponent

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
    ^.orient := "auto",
    ^.refX := "0",
    ^.refY := arrowHeadSize / 2,
    <.path(
      ^.d := s"M0,0 L0,${arrowHeadSize} L${arrowHeadSize},${arrowHeadSize / 2} z",
      ^.fill := "black"
    )
  )

  val BlackArrowHeadId = "blackarrowhead"
  val ReadArrowHeadId = "readarrowhead"

  val BlackArrowHead = (size: Double) =>  DroppedHead(size,BlackArrowHeadId,"black")//ArrowHead(size,BlackArrowHeadId,"black")
  val ReadArrowHead = (size: Double) =>  ArrowHead(size,ReadArrowHeadId,"red")

  private val DroppedHead = ScalaComponent.builder[(Double,String,String)]("DroppedHead")
    .render_P {
      case (size,id,color) =>
        <.marker(
          ^.id := id,
          ^.markerWidth := size,
          ^.markerHeight := size,
          ^.markerUnits := "userSpaceOnUse",
          ^.refX := size,
          ^.refY := size / 2,
          ^.orient := "auto",
          <.path(
            ^.d := s"M0,0 L0,${size} L${size},${size / 2} z",
            ^.fill := color
          )
        )
    }.build

  private val ArrowHead = ScalaComponent.builder[(Double,String,String)]("ArrowHead")
    .render_P { case (size,id,color) =>
      <.marker(
        ^.id := id,
        ^.markerWidth := size,
        ^.markerHeight := size,
        ^.markerUnits := "userSpaceOnUse",
        ^.refX := size,
        ^.refY := size / 2,
        ^.orient := "auto",
        <.path(
          ^.d := s"M0,0 L0,${size} L${size},${size / 2} z",
          ^.fill := color
        )
      )
    }.build

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
