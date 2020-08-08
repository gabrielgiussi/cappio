package oss.giussi.cappio.impl

import oss.giussi.cappio.Messages.{LocalRequest, LocalStep}
import oss.giussi.cappio.crdt.VectorTime
import oss.giussi.cappio.{AbstractModule, ModS, Module, ProcessId, ProcessLocalHelper1, StateWithModule}
import oss.giussi.cappio.crdt.pure.impl.AWSetService.AWSet
import oss.giussi.cappio.crdt.pure.impl.{AWSetService, AddOp, ClearOp, RemoveOp, SetOp}
import oss.giussi.cappio.impl.PhotosApp.{AddPhoto, Album, AlbumOp, AlbumOpResult, CreateAlbum}
import oss.giussi.cappio.impl.bcast.WaitingCausalBroadcast
import oss.giussi.cappio.impl.bcast.WaitingCausalBroadcast.{WCBMod, WCBroadcast, WCDeliver}

object AlbumCRDTApp {

  val ops = oss.giussi.cappio.crdt.pure.impl.AWSetService.AWSetServiceOps[String]

  sealed trait AlbumSetOp

  case class CreateOp(name: String) extends AlbumSetOp

  case class Op(name: String, op: SetOp) extends AlbumSetOp

  type AlbumCRDTDep = WCBMod[AlbumSetOp]

  type AlbumCRDTMod = ModS[AlbumCRDTDep] {
    type S = AlbumsCRDT
    type Ind = AlbumOpResult
    type Req = AlbumOp
  }

  case class AlbumCRDT(name: String, photos: AWSet[String]) {
    def doOp(op: SetOp, vt: VectorTime) = copy(photos = ops.effect(photos,op,vt))
  }

  case class AlbumsCRDT(collection: Map[String,AlbumCRDT]) {
    def addAlbum(name: String) = copy(collection + (name -> AlbumCRDT(name, AWSetService.zero[String])))

    def doOp(album: String, photoOp: SetOp, vt: VectorTime) = collection.get(album) match {
      case Some(a) => copy(collection.updated(album,a.doOp(photoOp,vt)))
      case None => this
    }

    def album(name: String) = collection.get(name)

    def contains(album: Album): LookupResult = collection.get(album.name) match {
      case None => NotFound
      case Some(a) if a == album => Match
      case _ => NotMatch
    }
  }

  object AlbumCRDTState {
    def init(all: Set[ProcessId], timeout: Int)(self: ProcessId) = StateWithModule(WaitingCausalBroadcast[AlbumSetOp](all,timeout)(self), AlbumsCRDT(Map()))
  }

  def processLocal = new ProcessLocalHelper1[AlbumCRDTMod,AlbumCRDTDep] {
    override def onPublicRequest(req: AlbumOp, state: State): Output = {
      val op = req match {
        case CreateAlbum(name) => CreateOp(name)
        case AddPhoto(albumId, photo) => Op(albumId,AddOp(photo))
      }
      LocalStep.withRequests(Set(LocalRequest(WCBroadcast(op))),state)
    }

    override def onIndication(ind: DInd, state: State): Output = {
      val s = ind.msg match {
        case CreateOp(name) => state.updateState(_.addAlbum(name))
        case Op(name,o) => state.updateState(_.doOp(name,o,ind.timestamp))
      }
      LocalStep.withState(s) // TODO indications
    }
  }


  def apply[P](all: Set[ProcessId], timeout: Int)(self: ProcessId): Module[AlbumCRDTMod] = {
    AbstractModule.mod[AlbumCRDTMod,AlbumCRDTDep](AlbumCRDTState.init(all, timeout)(self),processLocal)
  }
}
