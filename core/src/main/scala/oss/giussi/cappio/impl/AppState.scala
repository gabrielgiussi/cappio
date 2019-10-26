package oss.giussi.cappio.impl

import oss.giussi.cappio.Messages.{LocalRequest, LocalStep}
import oss.giussi.cappio.{AbstractModule, Messages, Mod, ModS, Module, ProcessLocalHelper1, StateWithModule}

object AppState {

  type AppMod2[P, D <: Mod] = ModS[D] {
    type Ind = D#Ind
    type Req = D#Req
    type S = AppState[P,D]
  }

  def app[P, D <: Mod](dependency: Module[D], f: Messages.ProcessLocalM[AppMod2[P,D],AppMod2[P,D]#Dep]): Module[AppMod2[P,D]] = {
    AbstractModule.mod[AppMod2[P,D],AppMod2[P,D]#Dep](AppState(None,dependency),f)
  }

  def app2[P, D <: Mod](dependency: Module[D], f: (AppMod2[P,D]#State,D#Ind) => AppMod2[P,D]#State): Module[AppMod2[P,D]] = {
    AbstractModule.mod[AppMod2[P,D],AppMod2[P,D]#Dep](AppState(None,dependency),new ProcessLocalHelper1[AppMod2[P,D],D] {
      override def onPublicRequest(req: D#Req, state: State): Output = LocalStep.withRequests(Set(LocalRequest(req)),state)

      override def onIndication(ind: DInd, state: State): Output = LocalStep.withIndications(Set(ind),f(state,ind))
    })
  }

}

case class AppState[S, M <: Mod](value: Option[S], module: Module[M]) extends StateWithModule[M,AppState[S,M]]{
  override def updateModule(m: Module[M]): AppState[S, M] = copy(module = m)

  def update(v: S) = copy(value = Some(v))
}
