package oss.giussi.cappio.impl

import oss.giussi.cappio.Messages.{LocalRequest, LocalStep}
import oss.giussi.cappio.impl.bcast.BestEffortBroadcast
import oss.giussi.cappio.impl.bcast.BestEffortBroadcast.{BebBcast, BebMod}
import oss.giussi.cappio.{AbstractModule, Messages, Mod, ModS, Module, ProcessId, ProcessLocalHelper1, StateWithModule}

object AppState {

  type AppMod2[P, D <: Mod] = ModS[D] {
    type Ind = D#Ind
    type Req = D#Req
    type S = Option[P] // TODO should be AppState so I know how to model it in showDOM
  }

  def apply[M <: Mod,P](module: Module[M]) = StateWithModule(module,Option.empty[P])

  def app[P, D <: Mod](dependency: Module[D], f: Messages.ProcessLocalM[AppMod2[P,D],AppMod2[P,D]#Dep]): Module[AppMod2[P,D]] = {
    AbstractModule.mod[AppMod2[P,D],AppMod2[P,D]#Dep](AppState(dependency),f)
  }

  def app2[P, D <: Mod](dependency: Module[D], f: (AppMod2[P,D]#S,D#Ind) => AppMod2[P,D]#S): Module[AppMod2[P,D]] = {
    AbstractModule.mod[AppMod2[P,D],AppMod2[P,D]#Dep](AppState(dependency),new ProcessLocalHelper1[AppMod2[P,D],D] {
      override def onPublicRequest(req: D#Req, state: State): Output = LocalStep.withRequests(Set(LocalRequest(req)),state)

      override def onIndication(ind: DInd, state: State): Output = LocalStep.withIndications(Set(ind),state.updateState(f(state.state,ind)))
    })
  }

}

sealed trait LookupResult
case object NotFound extends LookupResult
case object NotMatch extends LookupResult
case object Match extends LookupResult

object PhotosApp {

  case class Album(name: String, photos: Set[String]){
    def addPhoto(photo: String) = copy(photos = photos + photo)

    def removePhoto(photo: String) = copy(photos = photos - photo)
  }

  case class Albums(collection: Map[String,Album]) {
    def addAlbum(name: String) = copy(collection + (name -> Album(name, Set())))

    def addPhoto(album: String, photo: String) = collection.get(album) match {
      case Some(a) => copy(collection.updated(album,a.addPhoto(photo)))
      case None => this
    }

    def removePhoto(album: String, photo: String) = collection.get(album) match {
      case Some(a) => copy(collection.updated(album,a.removePhoto(photo)))
      case None => this
    }

    def album(name: String) = collection.get(name)

    def contains(album: Album): LookupResult = collection.get(album.name) match {
      case None => NotFound
      case Some(a) if a == album => Match
      case _ => NotMatch
    }
  }

  sealed trait AlbumOp

  case class CreateAlbum(name: String) extends AlbumOp

  case class AddPhoto(albumId: String, photo: String) extends AlbumOp

  case class RemovePhoto(albumId: String, photo: String) extends AlbumOp

  case class ClearAlbum(albumId: String) extends AlbumOp

  sealed trait AlbumOpResult

  case class AlbumCreated(name: String) extends AlbumOpResult

  case class PhotoCreated(album: String, photo: String) extends AlbumOpResult

  case class PhotoRemoved(album: String, photo: String) extends AlbumOpResult

  type AlbumAppMod[D <: Mod] = ModS[D] {
    type Ind = AlbumOpResult
    type Req = AlbumOp
    type S = Albums
  }

  def app2[P, D <: Mod](dependency: Module[D], f: (AlbumAppMod[D]#S,D#Ind) => AlbumAppMod[D]#S): Module[AlbumAppMod[D]] = {
    AbstractModule.mod[AlbumAppMod[D],AlbumAppMod[D]#Dep](StateWithModule(dependency,Albums(Map.empty)),new ProcessLocalHelper1[AlbumAppMod[D],D] {
      override def onPublicRequest(req: AlbumOp, state: State): Output = ???

      override def onIndication(ind: DInd, state: State): Output = ??? // LocalStep.withIndications(Set(ind),state.updateState(f(state.state,ind)))
    })
  }

  type AlbumBeb = AlbumAppMod[BebMod[AlbumOp]]
  def bestEffort(all: Set[ProcessId], timeout: Int)(self: ProcessId): Module[AlbumBeb] = {
    val beb = BestEffortBroadcast[AlbumOp](all,timeout)(self)

    AbstractModule.mod[AlbumBeb, AlbumBeb#Dep](StateWithModule(beb, Albums(Map.empty)), new ProcessLocalHelper1[AlbumBeb,AlbumBeb#Dep] {
      override def onPublicRequest(req: AlbumOp, state: State): Output = LocalStep.withRequests(Set(LocalRequest(BebBcast(req))),state)

      override def onIndication(ind: DInd, state: State): Output = ind.payload.msg match {
        case CreateAlbum(name) => LocalStep.withIndications(Set(AlbumCreated(name)),state.updateState(_.addAlbum(name)))
        case AddPhoto(album,photo) => LocalStep.withIndications(Set(PhotoCreated(album,photo)),state.updateState(_.addPhoto(album,photo)))
        case RemovePhoto(album,photo) => LocalStep.withIndications(Set(PhotoRemoved(album,photo)),state.updateState(_.removePhoto(album,photo)))
      }
    })
  }

}