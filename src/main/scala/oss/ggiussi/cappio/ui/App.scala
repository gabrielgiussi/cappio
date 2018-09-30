package oss.ggiussi.cappio.ui

import japgolly.scalajs.react.ScalaComponent
import japgolly.scalajs.react.extra.Reusability
import japgolly.scalajs.react.extra.router.{BaseUrl, Redirect, Resolution, Router, RouterConfigDsl, RouterCtl}
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom
import org.scalajs.dom.document
import oss.ggiussi.cappio.{Action}
import oss.ggiussi.cappio.Level.Condition
import oss.ggiussi.cappio.n.Composer.STuple3
import oss.ggiussi.cappio.n.FLLProtocol.{Deliver, Send}
import oss.ggiussi.cappio.n._
import oss.ggiussi.cappio.ui.n.LevelBackend


object App {

  sealed trait MyPages

  case object Home extends MyPages

  case object Level1 extends MyPages

  case object Hello extends MyPages

  case object GridPage extends MyPages

  case object LevelPage extends MyPages

  def level() = {
    implicit val payloads = Payloads(Set(1, 2),10)

    import Composer._

    type State = (STuple6[FairLossLinkStateN], STuple3[ProcessState])

    val processes: Option[Automaton[STuple3[ProcessState]]] = for {
      c1 <- ProcessN(0, Set(1,2)) composeTuple ProcessN(1, Set(0, 2)) // FIXME los neighbors los necesito para poder crear las input actions....
      c2 <- composeTuple2(c1, ProcessN(2, Set(1,0)))
    } yield c2

    val links: Option[Automaton[STuple6[FairLossLinkStateN]]] = for {
      c1 <- FairLossLinkN(0, 1) composeTuple FairLossLinkN(1, 0)
      c2 <- composeTuple2(c1, FairLossLinkN(1, 2))
      c3 <- composeTuple3(c2, FairLossLinkN(2, 1))
      c4 <- composeTuple4(c3, FairLossLinkN(0, 2))
      c5 <- composeTuple5(c4, FairLossLinkN(2, 0))
    } yield c5

    val automaton: Option[Automaton[State]] = for {
      l <- links
      p <- processes
      a <- l composeTuple p
    } yield a

    val initalState: State = (
      (FairLossLinkStateN.empty, FairLossLinkStateN.empty, FairLossLinkStateN.empty, FairLossLinkStateN.empty, FairLossLinkStateN.empty, FairLossLinkStateN.empty),
      (Up(0), Up(0), Up(0))
    )

    val conditions: List[Condition[State]] = List(
      (s: State) => s._2 match {
        case (Up(1), Up(1), Up(1)) => true
        case _ => false
      },
      (s: State) => List(s._1._1, s._1._2, s._1._3, s._1._4).forall(_.messages.collect { case e: Deliver => e }.isEmpty)
    )

    val schedConditions: List[Condition[List[Action]]] = List(
      (sched: List[Action]) => sched.exists {
        //case Send(0,1,2) => true TODO
        case _ => false
      }
    )

    LevelN(conditions,schedConditions, automaton.get, initalState)
  }

  def main(args: Array[String]): Unit = {

    val baseUrl: BaseUrl =
      if (dom.window.location.hostname == "localhost")
        BaseUrl.fromWindowUrl(s => s)
      else
        BaseUrl.fromWindowOrigin / "scalajs-react/"

    val navMenu = ScalaComponent.builder[RouterCtl[MyPages]]("Menu")
      .render_P { ctl =>
        def nav(name: String, target: MyPages) =
          <.li(
            ^.cls := "navbar-brand active",
            ctl setOnClick target,
            name)

        <.div(
          ^.cls := "navbar navbar-default",
          <.ul(
            ^.cls := "navbar-header",
            nav("Home", Home),
            nav("GridPage", GridPage),
            nav("Level1", LevelPage)
          ))
      }
      .configure(Reusability.shouldComponentUpdate)
      .build

    def layout(c: RouterCtl[MyPages], r: Resolution[MyPages]) =
      <.div(
        navMenu(c),
        <.div(^.cls := "container", r.render()))

    val routerConfig = RouterConfigDsl[MyPages].buildConfig { dsl =>
      import dsl._

      (emptyRule
        | staticRoute(root, Home) ~> render(<.label("CappIO"))
        | staticRoute("#level", LevelPage) ~> render(LevelBackend(level())())
        | staticRedirect("#hey") ~> redirectToPage(Hello)(Redirect.Replace)
        ).notFound(redirectToPage(Home)(Redirect.Replace))
        .renderWith(layout)
    }

    //val router = Router(BaseUrl.fromWindowOrigin / "my_page", routerConfig)
    val router = Router(baseUrl, routerConfig.logToConsole)
    router().renderIntoDOM(document.getElementById("playground"))
  }

}
