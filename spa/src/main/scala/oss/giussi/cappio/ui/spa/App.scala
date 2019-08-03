package oss.giussi.cappio.ui.spa

import com.raquo.laminar.api.L._
import com.raquo.laminar.nodes.ReactiveHtmlElement
import org.scalajs.dom
import org.scalajs.dom.{document, html}
import oss.giussi.cappio.ui.levels.{Documentation, IndexedLevel, Level, Levels}

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

      val hashes = EventStream.merge(EventStream.fromValue(dom.window.location.hash, true), windowEvents.onHashChange.map(_.newURL))
        .filter(_.contains("#")).map(_.split("#")(1))

      val levelSelection: EventStream[IndexedLevel] = hashes.map(x =>
        for {
          index <- Try(x.toInt).toOption
          level <- Levels.INDEXED_LEVELS.get(index)
        } yield level
      ).filter(_.isDefined).map(_.get)

      val levels = Levels.LEVELS.map { l =>
        val icon = l match {
          case IndexedLevel(_,Documentation(_)) => "fa-book"
          case _ => "fa-cogs"
        }
        a(href := s"#${l.x}", className := "list-group-item list-group-item-action waves-effect",
          className <-- levelSelection.map { active => if (active.x == l.x) "active" else "" },
          i(className := s"fas $icon mr-3", " Level " + l.x)
        )
      }

      val leftMenu: ReactiveHtmlElement[html.Div] = div(
        className := "list-group list-group-flush",
        levels
      )

      documentEvents.onDomContentLoaded.foreach { _ =>

        def getElementById(id: String, clean: Boolean = false) = {
          val container = document.getElementById(id)
          if (clean) container.textContent = ""
          container
        }

        render(getElementById("main-container", true), div(
          child <-- levelSelection.map(_.s.render)
        ))
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