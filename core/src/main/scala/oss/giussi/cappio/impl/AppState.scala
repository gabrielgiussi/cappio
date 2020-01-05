package oss.giussi.cappio.impl

import oss.giussi.cappio.Messages.{LocalRequest, LocalStep}
import oss.giussi.cappio.{AbstractModule, Messages, Mod, ModS, Module, ProcessLocalHelper1, StateWithModule}

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
