package oss.giussi.cappio.ui

import com.raquo.laminar.api.L._
import com.raquo.laminar.nodes.ReactiveHtmlElement
import org.scalajs.dom
import org.scalajs.dom.{document, html}
import oss.giussi.cappio.ui.levels.{IndexedLevel, Level, Levels}
import scala.util.Try


object App {

  case class Route(path: String)

  def main(args: Array[String]): Unit = {
    try {

      /*
    // https://github.com/japgolly/scalajs-react/blob/master/doc/ROUTER.md
    // https://github.com/japgolly/scalajs-react/tree/master/extra/src/main/scala/japgolly/scalajs/react/extra/router
    https://groups.google.com/forum/#!topic/scala-js/7s3f51uIn4w
    val routes: EventStream[Route] = ???
*/

      val hashes = EventStream.merge(EventStream.fromValue(dom.window.location.hash, true), windowEvents.onHashChange.map(_.newURL)).map(_.split("#")(1))

      // TODO how to update the hash if the current hash is not a valid number? contramap?
      val levelSelection: EventStream[IndexedLevel] = hashes.map(e => Try(e.toInt)).filter(_.isSuccess).map(_.get).filter(Levels.levels.map(_.x).contains).map(i => Levels.levels(i - 1))

      val levels = Levels.levels.map(l =>
        a(href := s"#${l.x}", className := "list-group-item waves-effect",
          className <-- levelSelection.map { active => if (active.x == l.x) "active" else "" },
          i(className := "fas fa-chart-pie mr-3", " Level " + l.x),
        )
      )

      val leftMenu: ReactiveHtmlElement[html.Div] = div(
        className := "list-group list-group-flush",
        levels
      )

      def router(levelSelection: EventStream[Level]) = {
        div(cls := "container-fluid mt-5",
          div(cls := "row wow fadeIn",
            div(cls := "col-md-9 mb-4",
              div(cls := "card",
                div(cls := "card-body",
                  child <-- levelSelection.map(_.diagram)
                )
              )
            ),
            div(
              cls := "col-md-3 mb-4",
              div(cls := "card mb-4",
                div(cls := "card-header text-center", "Action Selection"),
                div(cls := "card-body", child <-- levelSelection.map(_.actionSelection))
              )
            )
          ),
          div(
            child <-- levelSelection.map(_.states)
          )
        )
      }

      documentEvents.onDomContentLoaded.foreach { _ =>

        def getElementById(id: String, clean: Boolean = false) = {
          val container = document.getElementById(id)
          if (clean) container.textContent = ""
          container
        }

        render(getElementById("main-container", true), router(levelSelection.map(_.createLevel())))
        render(getElementById("sidebar"), leftMenu)

      }(unsafeWindowOwner)
    }
    catch {
      case e: Throwable =>
        e.printStackTrace()
        throw e
    }
  }

}