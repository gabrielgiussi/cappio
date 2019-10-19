package oss.giussi.cappio.impl

import oss.giussi.cappio.{AbstractModule, Messages, Mod, ModS, Module, StateWithModule}

object AppState {

  type AppMod2[P, D <: Mod] = ModS[D] {
    type Ind = D#Ind
    type Req = D#Req
    type S = AppState[P,D] // TODO if I use AppState[Mod#Payload,D] then I can't implement the process helper because the payload of the indication is T and the state is M#Payload
  }

  def app[P, D <: Mod](dependency: Module[D], f: Messages.ProcessLocalM[AppMod2[P,D],AppMod2[P,D]#Dep]): Module[AppMod2[P,D]] = {
    AbstractModule.mod[AppMod2[P,D],AppMod2[P,D]#Dep](AppState(None,dependency),f)
  }

}

case class AppState[S, M <: Mod](value: Option[S], module: Module[M]) extends StateWithModule[M,AppState[S,M]]{
  override def updateModule(m: Module[M]): AppState[S, M] = copy(module = m)

  def update(v: S) = copy(value = Some(v))
}
