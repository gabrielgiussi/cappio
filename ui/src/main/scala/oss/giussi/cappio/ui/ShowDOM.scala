package oss.giussi.cappio.ui

import com.raquo.laminar.api.L._
import oss.giussi.cappio.{BasicState, StateWithModule}
import oss.giussi.cappio.impl.AppState
import oss.giussi.cappio.impl.CRDTApp.CRDTState
import oss.giussi.cappio.impl.bcast.CausalOrderReliableBroadcast.{CORBDep, CORBMod, CRBData, CRBState}
import oss.giussi.cappio.impl.bcast.ReliableBroadcast.{RBDep, RBMod, RBcastState}
import oss.giussi.cappio.impl.bcast.UniformReliableBroadcast.URBState
import oss.giussi.cappio.impl.net.PerfectLink.{PLModule, PLState}
import oss.giussi.cappio.impl.net.StubLink
import oss.giussi.cappio.impl.net.StubbornLink.StubbornLinkState
import oss.giussi.cappio.impl.register.OneNRegularRegister.ONRRState
import oss.giussi.cappio.impl.time.PerfectFailureDetector.{PFDMod, PFDState}
import oss.giussi.cappio.{Mod => ModT}
import ShowDOMSyntax._
import ShowSyntax._

trait ShowDOM[A] {

  def toDOM(a: A): Div

}

trait GetState[A] {
  def getState(state: A): (String,Div)
}

object GetState {

  implicit def getAppState[M <: ModT,S](implicit s: Show[S]) = new GetState[AppState[S,M]] {
    override def getState(state: AppState[S, M]): (String, Div) = ("app",div(s"Valor actual: ${state.value.map(_.show).getOrElse("-")}"))
  }

  implicit def getPLState[P] = new GetState[PLState[P]] {
    override def getState(state: PLState[P]): (String, Div) = ("perfect link", div(s"Delivered ${state.delivered.size}"))
  }

  implicit def getCORBState[P] = new GetState[CORBMod[P]#State] {
    override def getState(state: CRBState[P]): (String, Div) = ("causal broadcast", div())
  }
}

object ShowDOM {

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

  def showStateWithModule[M <: ModT, S <: StateWithModule[M,S]](implicit get: GetState[S], show: ShowDOM[M#State]) = new ShowDOM[S] {
    override def toDOM(state: S): Div = stateWithModuleToDOM[M,S](state)
  }

  def stateWithModuleToDOM[M <: ModT, S <: StateWithModule[M,S]](state: S)(implicit get1: GetState[S], showDOM: ShowDOM[M#State]): Div = {
    val d = state.module.state.toDOM
    val (name,body) = get1.getState(state)
    d.insertChild(card(name, body),0)
    d
  }

  implicit def showAppState[P, M <: oss.giussi.cappio.Mod](implicit dep: ShowDOM[M#State], show: Show[P]): ShowDOM[AppState[P, M]] = showStateWithModule[M,AppState[P,M]]

  implicit def showCausal[P](implicit dep: ShowDOM[CORBDep[P]#State]) = new ShowDOM[CRBState[P]] {
    override def toDOM(a: CRBState[P]): Div = div(
      card("causal bcast",div("Delivered: ${a.delivered.size}")),
      //a.module.state.toDOM
    )
  }


  implicit def showComposed[D1,D2](implicit dep1: ShowDOM[D1], dep2: ShowDOM[D2]) = new ShowDOM[(D1,D2)] {
    override def toDOM(a: (D1, D2)): Div = ???
  }

  implicit def showRBMod[P] = new ShowDOM[RBMod[CRBData[P]]#State] {
    override def toDOM(a: RBcastState[CRBData[P]]): Div = ???
  }

  implicit def showCausal[P](implicit dep: CORBMod[P]#Dep#State) = showStateWithModule[CORBMod[P]#Dep,CORBMod[P]#State]

  implicit def showPL[P](implicit dep: ShowDOM[PLModule[P]#Dep#State]): ShowDOM[PLModule[P]#State] = showStateWithModule[PLModule[P]#Dep,PLModule[P]#State]

  implicit def showSL[P] = new ShowDOM[StubbornLinkState[P]] {
    override def toDOM(a: StubbornLinkState[P]): Div = div(card("stubborn link", div()))
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