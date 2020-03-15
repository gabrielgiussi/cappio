package oss.giussi.cappio.ui.levels.bcast

import com.raquo.laminar.api.L._
import oss.giussi.cappio.impl.PhotosApp
import oss.giussi.cappio.impl.PhotosApp.{AddPhoto, AlbumReliable, CreateAlbum}
import oss.giussi.cappio.impl.bcast.ReliableBroadcast._
import oss.giussi.cappio.ui.ActionSelection
import oss.giussi.cappio.ui.core.Index
import oss.giussi.cappio.ui.levels.Snapshot.Conditions
import oss.giussi.cappio.ui.levels.{AbstractLevel, Level, PredefinedAction}
import oss.giussi.cappio.{ProcessId, ProcessRequest, Scheduler}

object RBLevel {
  type ModLevel = AlbumReliable

  def scheduler[P](nProcesses: Int, timeout: Int): Scheduler[ModLevel] = {
    val all = (0 to nProcesses).map(ProcessId).toSet
    Scheduler.init(all,PhotosApp.reliable(all,timeout))
  }

  val rb = ActionSelection.payloadRequest("Broadcast")({ case (_,s) => RBBcast(s)}) _

  val ok = RBLevel(List(

  )) _

  //val ko = RBLevel(???)

  def apply(cond: Conditions[ModLevel])(nProcesses: Int, timeout: Int): Level[ModLevel] = new AbstractLevel[ModLevel](scheduler(nProcesses,timeout),cond) {
    override val indicationPayload = _.toString // TODO
    override val reqTypes = List(
      //crb,
      ActionSelection.crash
    )
    override val shortDescription = div()

    override def predefined: Set[PredefinedAction[Req]] = Set(
      PredefinedAction(Index(1),ProcessId(0),ProcessRequest.predefined(ProcessId(0), CreateAlbum("Mis fotos"))),
      PredefinedAction(Index(3),ProcessId(0),ProcessRequest.predefined(ProcessId(0), AddPhoto("Mis fotos", "Vacaciones"))),
      PredefinedAction(Index(5),ProcessId(0),ProcessRequest.predefined(ProcessId(0), AddPhoto("Mis fotos", "Graduacion")))
    )

    override def title: String = "Reliable"
  }

}