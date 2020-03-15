package oss.giussi.cappio.impl.bcast

import java.util.UUID

import oss.giussi.cappio.Messages.{LocalRequest, LocalStep}
import oss.giussi.cappio.impl.bcast.BestEffortBroadcast.{BebBcast, BebMod}
import oss.giussi.cappio.impl.bcast.ReliableBroadcast.{RBBcast, RBDeliver}
import oss.giussi.cappio.{AbstractModule, Instance, ModS, Module, ProcessId, ProcessLocalHelper1, StateWithModule}

object EagerReliableBroadcast {

  type ERBMod[P] = ModS[BebMod[P]] {
    type S = ERBcastState
    type Ind = RBDeliver[P]
    type Req = RBBcast[P]
  }

  object ERBcastState {
    def init[P](self: ProcessId, all: Set[ProcessId], timeout: Int): StateWithModule[BebMod[P],ERBcastState] = StateWithModule(BestEffortBroadcast(all,timeout)(self),ERBcastState(Set()))
  }

  case class ERBcastState(delivered: Set[UUID]) {
    def deliver(id: UUID) = copy(delivered + id)
  }

  def processLocal[P](self: ProcessId) = new ProcessLocalHelper1[ERBMod[P],BebMod[P]]{
    override def onPublicRequest(req: RBBcast[P], state: State): Output = LocalStep.withRequests(Set(LocalRequest(BebBcast(req.payload,Instance.ANY))),state)

    override def onIndication(ind: DInd, state: State): Output = {
      if (state.state.delivered.contains(ind.payload.id)) LocalStep.withState(state)
      else {
        LocalStep.withRequestsAndIndications(Set(RBDeliver(ind.from,ind.payload)),Set(LocalRequest(BebBcast(ind.payload,Instance.ANY))),state.updateState(_.deliver(ind.payload.id)))
      }
    }
  }

  def apply[T](all: Set[ProcessId], timeout: Int)(self: ProcessId): Module[ERBMod[T]] = {
    AbstractModule.mod[ERBMod[T],ERBMod[T]#Dep](ERBcastState.init[T](self,all,timeout),processLocal[T](self))
  }

}
