package oss.giussi.cappio.ui.levels.bcast

import com.raquo.laminar.api.L._
import oss.giussi.cappio.impl.AlbumCRDTApp
import oss.giussi.cappio.impl.AlbumCRDTApp.AlbumCRDTMod
import oss.giussi.cappio.impl.CRDTApp.{Add, SetRequest}
import oss.giussi.cappio.impl.PhotosApp.{AddPhoto, CreateAlbum}
import oss.giussi.cappio.ui.ActionSelection
import oss.giussi.cappio.ui.core.Index
import oss.giussi.cappio.ui.levels.Snapshot.Conditions
import oss.giussi.cappio.ui.levels.{AbstractLevel, Level, PredefinedAction}
import oss.giussi.cappio.{ProcessId, ProcessRequest, Scheduler}

object CRDTLevel {

  type ModLevel = AlbumCRDTMod

  val add = ActionSelection.payloadRequest[SetRequest]("Add"){ case (_,payload) => Add(payload) } _

  val conditions: Conditions[ModLevel] = List()

  def scheduler(nProcesses: Int, timeout: Int): Scheduler[ModLevel] = {
    val all = (0 to nProcesses).map(ProcessId).toSet
    Scheduler.init(all,AlbumCRDTApp(all,timeout))
  }

  def apply(cond: Conditions[ModLevel])(nProcesses: Int, timeout: Int): Level[ModLevel] = new AbstractLevel[ModLevel](scheduler(nProcesses,timeout),cond) {
    override val indicationPayload = _.toString
    override val reqTypes = List(
      ActionSelection.crash
    )
    override val shortDescription = div()

    override def title: String = "TODO"

    override def predefined: Set[PredefinedAction[Req]] = Set(
      PredefinedAction(Index(1),ProcessId(0),ProcessRequest.predefined(ProcessId(0), CreateAlbum("Mis fotos"))),
      PredefinedAction(Index(3),ProcessId(0),ProcessRequest.predefined(ProcessId(0), AddPhoto("Mis fotos", "Vacaciones"))),
      PredefinedAction(Index(5),ProcessId(0),ProcessRequest.predefined(ProcessId(0), AddPhoto("Mis fotos", "Graduacion")))
    )

  }

  val ok = CRDTLevel(List()) _

}
