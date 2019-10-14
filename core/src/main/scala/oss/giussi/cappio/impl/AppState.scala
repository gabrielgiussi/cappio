package oss.giussi.cappio.impl

import oss.giussi.cappio.{AbstractModule, Messages, Mod, ModS, Module, StateWithModule}

object AppState {

  type AppMod2[P,D <: Mod { type Payload = P}] = ModS[D] {
    type Ind = D#Ind
    type Req = D#Req
    type S = AppState[P,D]
  }

  def app[T,D <: Mod { type Payload = T}](dependency: Module[D], f: Messages.ProcessLocalM[AppMod2[T,D],AppMod2[T,D]#Dep]): Module[AppMod2[T,D]] = {
    AbstractModule.mod[AppMod2[T,D],AppMod2[T,D]#Dep](AppState(None,dependency),f)
  }

}

case class AppState[S, M <: Mod](value: Option[S], module: Module[M]) extends StateWithModule[M,AppState[S,M]]{
  override def updateModule(m: Module[M]): AppState[S, M] = copy(module = m)

  def update(v: S) = copy(value = Some(v))
}
