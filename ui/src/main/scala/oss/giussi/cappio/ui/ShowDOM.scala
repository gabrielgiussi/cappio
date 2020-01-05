package oss.giussi.cappio.ui

import com.raquo.laminar.api.L._
import oss.giussi.cappio.crdt.Versioned
import oss.giussi.cappio.crdt.pure.impl.AWSetService.AWSet
import oss.giussi.cappio.impl.CRDTApp.CRDTMod
import oss.giussi.cappio.impl.bcast.CausalOrderReliableBroadcast.{CORBMod, CRBState, CausalApp}
import oss.giussi.cappio.impl.bcast.ReliableBroadcast.{RBApp, RBcastState}
import oss.giussi.cappio.impl.bcast.UniformReliableBroadcast.URBState
import oss.giussi.cappio.impl.bcast.WaitingCausalBroadcast.WCBState
import oss.giussi.cappio.impl.net.PerfectLink.PLStateInternal
import oss.giussi.cappio.impl.net.StubbornLink.StubbornLinkState
import oss.giussi.cappio.impl.time.PerfectFailureDetector.PFDState
import oss.giussi.cappio.ui.ShowDOMSyntax._
import oss.giussi.cappio.ui.ShowSyntax._
import oss.giussi.cappio.{NoState, StateWithModule, Mod => ModT}

trait ShowDOM[A] {

  def toDOM(a: A): Div

}

object ShowDOM {

  def card(header: String, body: Div) = div(cls := "card my-2", id := header,
    div(cls := "card-header py-0 px-1",
      fontSize.small,
      textAlign.right,
      header
    ),
    div(cls := "card-body py-2",
      body
    )
  )

  // TODO
  implicit def showVersioned[P] = new ShowDOM[Versioned[P]] {
    override def toDOM(a: Versioned[P]): Div = div("versioned")
  }


  implicit def showComposed[D1, D2](implicit dep1: ShowDOM[D1], dep2: ShowDOM[D2]) = new ShowDOM[(D1, D2)] {
    override def toDOM(a: (D1, D2)): Div = div(cls := "container",
      div(cls := "row",
        div(cls := "col-sm px-0 pr-2", a._1.toDOM),
        div(cls := "col-sm px-0 pl-2", a._2.toDOM)
      )
    )
  }

  // Should receive a different thing for showState! (maybe a GetState)
  implicit def showStateWithModule[M <: ModT, S](implicit showDep: ShowDOM[M#State], showState: ShowDOM[S]) = new ShowDOM[StateWithModule[M, S]] {
    override def toDOM(a: StateWithModule[M, S]): Div = div(
      a.state.toDOM,
      a.module.state.toDOM
    )
  }

  implicit def showPL[P: Show] = new ShowDOM[PLStateInternal[P]] {
    override def toDOM(a: PLStateInternal[P]): Div = card("perfect link", div("a"))
  }

  implicit def showSL[P: Show] = new ShowDOM[StubbornLinkState[P]] {
    override def toDOM(a: StubbornLinkState[P]): Div = card("stubborn link", div())
  }

  implicit def showDOMUnit = new ShowDOM[NoState] {
    override def toDOM(a: NoState): Div = card(a.name, div())
  }

  implicit def showDOMOption[P](implicit show: Show[P]) = new ShowDOM[Option[P]] {
    override def toDOM(a: Option[P]): Div = card("app", div(s"Valor actual: ${a.map(_.show).getOrElse("-")}"))
  }

  implicit def showPFDState = new ShowDOM[PFDState] {
    override def toDOM(a: PFDState): Div = card("failure detector", div(s"Crashed ${a.detected.size}"))
  }

  implicit def showRBcastState[P: Show] = new ShowDOM[RBcastState[P]] {
    override def toDOM(a: RBcastState[P]): Div = card("reliable bcast", div(
      div(s"Correct: ${a.correct.size}"),
      div(s"Delivered ${a.delivered.values.headOption.flatMap(_.headOption.map(_.msg.show))}") // TODO
    ))
  }

  // FIXME require Show[P]
  implicit def showCausalState[P] = new ShowDOM[CRBState[P]] {
    override def toDOM(a: CRBState[P]): Div = ???
  }

  implicit def showURBState[P] = new ShowDOM[URBState[P]] {
    override def toDOM(a: URBState[P]): Div = ???
  }

  // FIXME not only for string
  implicit def showAWSet = new ShowDOM[AWSet[String]] {
    override def toDOM(a: AWSet[String]): Div = card("app", div("aw-set"))
  }

  implicit def showWCBState[P] = new ShowDOM[WCBState[P]] {
    override def toDOM(a: WCBState[P]): Div = card("waiting causal bcast", div("waiting"))
  }

  implicit def showRB[P: Show]: ShowDOM[RBApp[P]#State] = showStateWithModule[RBApp[P]#Dep, RBApp[P]#S](implicitly, showDOMOption[P]) // TODO

  implicit def showCausal[P: Show]: ShowDOM[CausalApp[P]#State] = {
    val a: ShowDOM[CausalApp[P]#Dep#State] = showStateWithModule[CausalApp[P]#Dep#State#Dep, CausalApp[P]#Dep#State#State](implicitly, implicitly)
    showStateWithModule[CausalApp[P]#Dep, CausalApp[P]#S](a, showDOMOption[P])
  }

  implicit def showCRDT[P]: ShowDOM[CRDTMod#State] = {
    val a: ShowDOM[CRDTMod#Dep#State] = showStateWithModule[CRDTMod#Dep#State#Dep, CRDTMod#Dep#State#State](implicitly, implicitly)
    showStateWithModule[CRDTMod#Dep, CRDTMod#S](a, implicitly)
  }

}

object ShowDOMSyntax {

  implicit class ShowDOMOps[A](val a: A) extends AnyVal {
    def toDOM(implicit ev: ShowDOM[A]): Div = ev.toDOM(a)
  }

}