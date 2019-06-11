package oss.giussi.cappio.ui

import java.util.UUID

import com.raquo.airstream.signal.Signal
import com.raquo.laminar.api.L.{svg => s, _}
import com.raquo.laminar.nodes.ReactiveHtmlElement
import org.scalajs.dom
import org.scalajs.dom.ext.KeyCode
import org.scalajs.dom.raw.HashChangeEvent
import org.scalajs.dom.{document, html}
import oss.giussi.cappio.{ProcessId, Processes}
import oss.giussi.cappio.ui.core.{Action, Crashed, Delivered, Dropped, Index, Indication, Request, Undelivered}

import scala.util.Try


object SampleMain {

  case class Point(x: Int, y: Int)

  case class Route(path: String)

  def main(args: Array[String]): Unit = {

    /*
    // https://github.com/japgolly/scalajs-react/blob/master/doc/ROUTER.md
    // https://github.com/japgolly/scalajs-react/tree/master/extra/src/main/scala/japgolly/scalajs/react/extra/router
    https://groups.google.com/forum/#!topic/scala-js/7s3f51uIn4w
    val routes: EventStream[Route] = ???
*/

    val hashes = EventStream.merge(EventStream.fromValue(dom.window.location.hash,true),windowEvents.onHashChange.map(_.newURL)).map(_.split("#")(1))

    // TODO how to update
    val levelSelection: EventStream[Level] = hashes.map(e => Try(e.toInt)).filter(_.isSuccess).map(_.get).filter(Levels.levels.map(_.x).contains).map(i => Levels.levels(i - 1))

    val levels = Levels.levels.map(l =>
      a(href := s"#${l.x}", className := "list-group-item waves-effect",
        className <-- levelSelection.map { active => if (active.x == l.x) "active" else "" },
        i(className := "fas fa-chart-pie mr-3", " Level " + l.x),
        //onClick.mapTo(l) --> levelSelection
      )
    )

    val leftMenu: ReactiveHtmlElement[html.Div] = div(
      className := "list-group list-group-flush",
      levels
    )

    //dom.window.history.


    val uuid = UUID.randomUUID()

    val actions: List[Action] = List(
      Undelivered(ProcessId(0), ProcessId(2), uuid, "", Index(0)),
      Delivered(ProcessId(0), ProcessId(2), uuid, "", Index(0), Index(2)),
      Delivered(ProcessId(1), ProcessId(0), UUID.randomUUID(), "", Index(3), Index(4)),
      Delivered(ProcessId(2), ProcessId(0), UUID.randomUUID(), "", Index(6), Index(9)),
      Crashed(ProcessId(2),Index(5)),
      Request(ProcessId(0),Index(6),""),
      Indication(ProcessId(1),Index(20),""),
      Dropped(ProcessId(0),ProcessId(1),UUID.randomUUID(),"",Index(25),Index(27))
    )

    val clicks = new EventBus[String]
    val $actions: EventStream[List[Action]] = clicks.events.fold(0)((acc,command) => if (command == "Next") acc + 1 else acc - 1).map(i => actions.take(i)
    .groupBy(_.id).map(_._2.last).toList
    ).changes

    val prevButton = button(
      "Prev",
      onClick.preventDefault.mapToValue("Prev") --> clicks
    )
    val nextButton = button(
      "Next",
      onClick.preventDefault.mapToValue("Next") --> clicks
    )
    val buttons = div(prevButton,nextButton)

/*
    def levelLifetimes(level: Level, $conf: Signal[GridConf]) = {
      val lines = level.processes.ids.toList.map { case p =>
        val $p0 = $conf.map(_.p(Index(0), p))
        val $p1 = $conf.map(c => c.p(Index(10000), p))
        s.line(
          s.stroke := "black",
          s.strokeWidth := "3",
          s.x1 <-- $p0.map(_.x.toString),
          s.x2 <-- $p1.map(_.x.toString),
          s.y1 <-- $p0.map(_.y.toString),
          s.y2 <-- $p0.map(_.y.toString),
          s.r := "30"
        )
      }

      val height = "400"
      val width = "800"
      val x = 0 // move x to move right
      s.svg(
        s.height := height,
        s.width := width,
        s.viewBox := s"$x 0 $width $height",
        Markers.defs(conf.arrowHeadSize.toInt),
        lines,
        actionsSVG
      )
    }

 */

    val $conf = new EventBus[GridConf]

    val currentLevel = div(
      styleAttr := "overflow: scroll; overflow-y: hidden;", // FIXME esto deberia ir dentro de diagram
      child <-- levelSelection.map(l => Diagram(l.processes,$actions))
    )

    /*
    val form = input(
      inContext { thisNode =>
        // Note: mapTo below accepts parameter by-name, evaluating it on every enter key press
        // TODO no se si esta bien como limpio el value aca
        onKeyPress.filter(_.keyCode == KeyCode.Enter).mapTo(thisNode.ref.value).filter(_.nonEmpty).map { d => thisNode.ref.value = ""; conf.copy(roundHeight = d.toDouble) } --> $conf
      }
    )

     */

    documentEvents.onDomContentLoaded.foreach { _ =>

      val container = document.getElementById("app")
      container.textContent = ""

      render(container, div(
        currentLevel,
        buttons
      ))
      render(document.getElementById("sidebar"), leftMenu)
      //render(container, div(sched(), ActionSelection.bebBroadcast(ps,st.writer)))

    }(unsafeWindowOwner)
  }

}