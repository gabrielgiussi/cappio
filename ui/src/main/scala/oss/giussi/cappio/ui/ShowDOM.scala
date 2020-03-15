package oss.giussi.cappio.ui

import com.raquo.laminar.api.L._
import oss.giussi.cappio.crdt.Versioned
import oss.giussi.cappio.crdt.pure.impl.AWSetService
import oss.giussi.cappio.crdt.pure.impl.AWSetService.AWSet
import oss.giussi.cappio.impl.CRDTApp.CRDTMod
import oss.giussi.cappio.impl.PhotosApp.{AlbumBeb, AlbumCausal, Albums}
import oss.giussi.cappio.impl.bcast.CausalOrderReliableBroadcast.{CORBMod, CRBState, CausalApp}
import oss.giussi.cappio.impl.bcast.EagerReliableBroadcast.ERBcastState
import oss.giussi.cappio.impl.bcast.ReliableBroadcast.RBcastState
import oss.giussi.cappio.impl.bcast.UniformReliableBroadcast.URBState
import oss.giussi.cappio.impl.bcast.WaitingCausalBroadcast.{VersionedFrom, WCBMod, WCBState}
import oss.giussi.cappio.impl.net.PerfectLink.PLStateInternal
import oss.giussi.cappio.impl.net.StubbornLink.StubbornLinkState
import oss.giussi.cappio.impl.time.PerfectFailureDetector.PFDState
import oss.giussi.cappio.ui.ShowDOMSyntax._
import oss.giussi.cappio.ui.ShowSyntax._
import oss.giussi.cappio.{NoState, Packet, ProcessId, StateWithModule, Mod => ModT}

trait ShowDOM[A] {

  def toDOM(a: A): Div

}

object ShowDOM {

  def emptyCard(header: String) = div(cls := "card my-2", id := header,
    div(cls := "card-header py-0 px-1",
      fontSize.small,
      textAlign.right,
      header
    )
  )

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

  def packetList[P : Show](packets: Set[Packet[P]]) = ul(cls := "list-group",
    packets.toList.sortBy(_.id).take(3).map(packet => li(cls := "list-group-item p-0", packet.show))
  )

  def processArray(description: String, processes: Set[ProcessId]) = div(s"$description: [${processes.toList.sortBy(_.id).mkString(",")}]")


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
    override def toDOM(a: PLStateInternal[P]): Div = card("perfect link", div(
      "Entregados",
      packetList(a.delivered)
    ))
  }

  implicit def showSL[P: Show] = new ShowDOM[StubbornLinkState[P]] {
    override def toDOM(a: StubbornLinkState[P]): Div = card("stubborn link", div(
      div(s"Timer: ${a.timer}/${a.timeout}"),
      "Enviados",
      packetList(a.sent)
    ))
  }

  implicit def showDOMUnit = new ShowDOM[NoState] {
    override def toDOM(a: NoState): Div = emptyCard(a.name)
  }

  implicit def showDOMOption[P : Show] = new ShowDOM[Option[P]] {
    override def toDOM(a: Option[P]): Div = card("app", div(s"Valor actual: ${a.map(_.show).getOrElse("-")}"))
  }

  implicit def showPFDState = new ShowDOM[PFDState] {
    override def toDOM(a: PFDState): Div = card("failure detector", processArray("Procesos detectados", a.detected))
  }

  implicit def showRBcastState[P: Show] = new ShowDOM[RBcastState[P]] {
    override def toDOM(a: RBcastState[P]): Div = card("reliable bcast", div(
      processArray("Procesos correctos", a.correct)
      //div(s"Entregados ${a.delivered.values.headOption.flatMap(_.headOption.map(_.msg.show))}") TODO
    ))
  }

  implicit def showCausalState[P : Show] = new ShowDOM[CRBState[P]] {
    override def toDOM(a: CRBState[P]): Div = card("causal reliable broadcast", div(
      div(
        "Entregados",
        ul(cls := "list-group",
          a.delivered.toList.map(id => li(cls := "list-group-item p-0", id.toString))
        ),
      ),
      div(
        "Pasado",
        ul(cls := "list-group",
          a.past.list.map { case  (pid,id,pay) => li(cls := "list-group-item", s"($pid,$id,${pay.show})")}
        )
      )
    ))
  }

  implicit def showURBState[P : Show] = new ShowDOM[URBState[P]] {
    override def toDOM(a: URBState[P]): Div = card("uniform reliable broadcast", div(
      processArray("Procesos correctos", a.correct),
      div(
        "Entregados",
        ul(cls := "list-group",
          a.delivered.toList.map(id => li(cls := "list-group-item p-0", id.toString))
        )
      ),
      div(
        "Pendientes",
        ul(cls := "list-group",
          a.pending.toList.map { case (pid,id,pay) => li(cls := "list-group-item p-0", s"($pid,$id,${pay.show})")}
        )
      )
    ))
  }

  // FIXME not only for string
  // TODO show POLog
  implicit def showAWSet = new ShowDOM[AWSet[String]] {
    override def toDOM(a: AWSet[String]): Div = card("app", div(
      div(
        s"Valor actual: [${AWSetService.AWSetServiceOps.eval(a).mkString(",")}]"
      ),
    ))
  }

  implicit def showWCBState[P] = new ShowDOM[WCBState[P]] {
    override def toDOM(a: WCBState[P]): Div = card("waiting causal bcast", div(
      div("Vector Clock ",
        a.clock.show
      ),
      div(
        "Pendientes ",
        ul(cls := "list-group",
          a.pending.toList.map { case VersionedFrom(processId, Versioned(value, vectorTimestamp, _, _)) => li(cls := "list-group-item", s"${value} - ${vectorTimestamp.show}")}
        )

      )
    ))
  }

  implicit val showEReliableBcast = new ShowDOM[ERBcastState] {
    override def toDOM(a: ERBcastState): Div = card("eager reliable bcast",div("TODO"))
  }

  implicit def showCausal[P: Show]: ShowDOM[WCBMod[P]#State] = {
    val a: ShowDOM[WCBMod[P]#Dep#State] = showStateWithModule[WCBMod[P]#Dep#State#Dep, WCBMod[P]#Dep#State#State](implicitly, implicitly)
    showStateWithModule[WCBMod[P]#Dep, WCBMod[P]#S](a, implicitly)
  }

  /*
  implicit def showCRDT[P]: ShowDOM[CRDTMod#State] = {
    val a: ShowDOM[CRDTMod#Dep#State] = showStateWithModule[CRDTMod#Dep#State#Dep, CRDTMod#Dep#State#State](implicitly, implicitly)
    showStateWithModule[CRDTMod#Dep, CRDTMod#S](a, implicitly)
  }
   */

  implicit val showAlbums = new ShowDOM[Albums] {
    override def toDOM(a: Albums): Div = card("albums",div(
      if (a.collection.isEmpty) "La galeria esta vacia" else {
        ul(
          (a.collection.values.toList.map(album => li(s"${album.name}: [${album.photos.mkString(",")}]")))
        )
      }
    ))
  }

}

object ShowDOMSyntax {

  implicit class ShowDOMOps[A](val a: A) extends AnyVal {
    def toDOM(implicit ev: ShowDOM[A]): Div = ev.toDOM(a)
  }

}