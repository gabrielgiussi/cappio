package oss.giussi.cappio.ui

import com.raquo.laminar.api.L._
import oss.giussi.cappio.BasicState
import oss.giussi.cappio.impl.AppState
import oss.giussi.cappio.impl.CRDTApp.CRDTState
import oss.giussi.cappio.impl.bcast.CausalOrderReliableBroadcast.{CORBDep, CRBState}
import oss.giussi.cappio.impl.bcast.ReliableBroadcast.{RBDep, RBcastState}
import oss.giussi.cappio.impl.bcast.UniformReliableBroadcast.URBState
import oss.giussi.cappio.impl.net.PerfectLink.PLState
import oss.giussi.cappio.impl.net.StubLink
import oss.giussi.cappio.impl.net.StubbornLink.StubbornLinkState
import oss.giussi.cappio.impl.register.OneNRegularRegister.ONRRState
import oss.giussi.cappio.impl.time.PerfectFailureDetector.{PFDMod, PFDState}

trait ShowDOM[A] {

  def toDOM(a: A): Div

}

object ShowDOM {

  import ShowDOMSyntax._
  import ShowSyntax._

  def card(header: String, body: Div) = div(cls := "card my-2",
    div(cls := "card-header py-0 px-1",
      fontSize.small,
      textAlign.right,
      header
    ),
    div(cls := "card-body py-2",
      body
    )
  )

  implicit def showAppState[P, M <: oss.giussi.cappio.Mod](implicit dep: ShowDOM[M#State], show: Show[P]) = new ShowDOM[AppState[P, M]] {
    override def toDOM(a: AppState[P, M]): Div = {
      val d = a.module.state.toDOM
      d.insertChild(card("app", div(s"Valor actual: ${a.value.map(_.show).getOrElse("-")}")),0)
      d
    }
  }

  implicit def showCausal[P](implicit dep: ShowDOM[CORBDep[P]#State]) = new ShowDOM[CRBState[P]] {
    override def toDOM(a: CRBState[P]): Div = div(
      card("causal bcast",div("Delivered: ${a.delivered.size}")),
      //a.module.state.toDOM
    )
  }

  implicit def showPL[P](implicit dep: ShowDOM[StubLink[P]#State]) = new ShowDOM[PLState[P]] {
    override def toDOM(a: PLState[P]): Div = div(
      card("perfect link", div(s"Delivered ${a.delivered.size}"))
      //a.module.state.toDOM
    )
  }

  implicit def showSL[P] = new ShowDOM[StubbornLinkState[P]] {
    override def toDOM(a: StubbornLinkState[P]): Div = div(
      borderStyle := "solid",
      "Stubborn Link"
    )
  }

  implicit def showBasicState[M <: oss.giussi.cappio.Mod](implicit dep: ShowDOM[M#State]) = new ShowDOM[BasicState[M]] {
    override def toDOM(a: BasicState[M]): Div = div(
      div(
        borderStyle := "solid",
        "Best Effort Broadcast no tiene estado"
      ),
      a.module.state.toDOM
    )
  }

  implicit def showRB[P](implicit dep: ShowDOM[PFDMod#State]) = new ShowDOM[RBcastState[P]] {
    override def toDOM(a: RBcastState[P]): Div = a.module.state._1.toDOM
  }

  implicit def showURBState[P] = new ShowDOM[URBState[P]] {
    override def toDOM(a: URBState[P]): Div = div()
  }

  implicit def showONRRState[P] = new ShowDOM[ONRRState[P]] {
    override def toDOM(a: ONRRState[P]): Div = div()
  }

  implicit  def showPFDState = new ShowDOM[PFDState] {
    override def toDOM(a: PFDState): Div = div(
      label("dead: " + a.detected.mkString(","))
    )
  }

  implicit def showCRDTState = new ShowDOM[CRDTState] {
    import oss.giussi.cappio.crdt.pure.impl.AWSetService.AWSetServiceOps
    val ops = AWSetServiceOps[String]

    override def toDOM(a: CRDTState): Div = div(
      div(s"Pending: ${a.module.state.pending.size}"),
      div(s"Set: [${ops.eval(a.crdt).mkString(",")}]"),
      div(s"Dead: [${a.module.state.module.state.module.state._1.detected.mkString(",")}]")
    )
  }

}

object ShowDOMSyntax {

  implicit class ShowDOMOps[A](val a: A) extends AnyVal {
    def toDOM(implicit ev: ShowDOM[A]): Div = ev.toDOM(a)
  }

}