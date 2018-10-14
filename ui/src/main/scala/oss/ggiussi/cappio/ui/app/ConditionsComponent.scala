package oss.ggiussi.cappio.ui.app

import japgolly.scalajs.react.ScalaComponent
import japgolly.scalajs.react.vdom.html_<^._

case class ConditionsState[S](state: S, conditions: List[(String,S => Boolean)])

object ConditionsComponent {

  val Conditions = ScalaComponent.builder[List[(String, Boolean)]]("Conditions")
    .render_P(c =>
      <.ul(
        c.zipWithIndex.toVdomArray { case ((s, r),index) =>
          <.li(
            ^.key := index,
            <.label(
              ^.role := "alert",
              ^.cls := s"${if (r) "alert alert-success" else "alert alert-danger"}",
              s
            )
          )
        }
      )
    ).build

  def apply[S](conditions: List[(String, Boolean)]) = Conditions.apply(conditions)
}



