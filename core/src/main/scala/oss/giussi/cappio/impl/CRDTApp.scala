package oss.giussi.cappio.impl

import oss.giussi.cappio.Messages.{LocalRequest, LocalStep}
import oss.giussi.cappio.crdt.pure.impl.AWSetService.AWSet
import oss.giussi.cappio.crdt.pure.impl.{AWSetService, AddOp, RemoveOp, SetOp}
import oss.giussi.cappio.impl.bcast.WaitingCausalBroadcast
import oss.giussi.cappio.impl.bcast.WaitingCausalBroadcast.{WCBMod, WCBroadcast, WCDeliver}
import oss.giussi.cappio.{AbstractModule, ModS, Module, ProcessId, ProcessLocalHelper1, StateWithModule}

object CRDTApp {

  type CRDTDep = WCBMod[SetOp]

  type CRDTMod = ModS[CRDTDep] {
    type S = CRDTState
    type Req = SetRequest
    type Ind = WCDeliver[SetOp]
  }

  sealed trait SetRequest

  case class Add(payload: String) extends SetRequest

  case class Remove(payload: String) extends SetRequest


  object CRDTState {
    def init(all: Set[ProcessId], timeout: Int)(self: ProcessId): CRDTState = new CRDTState(WaitingCausalBroadcast(all,timeout)(self), AWSetService.zero[String])
  }

  case class CRDTState(module: Module[CRDTDep], crdt: AWSet[String]) extends StateWithModule[CRDTDep, CRDTState] {
    override def updateModule(m: Module[CRDTDep]): CRDTState = copy(module = m)
  }


  def processLocal = new ProcessLocalHelper1[CRDTMod,CRDTDep] {

    val ops = oss.giussi.cappio.crdt.pure.impl.AWSetService.AWSetServiceOps[String]

    // TODO should call prepare here?
    override def onPublicRequest(req: SetRequest, state: State) = {
      val op = req match {
        case Add(p) => AddOp(p)
        case Remove(p) => RemoveOp(p)
      }
      LocalStep.withRequests(Set(LocalRequest(WCBroadcast(op))),state)
    }

    override def onIndication(ind: DInd, state: State) = {
      val ns = state.copy(crdt = ops.effect(state.crdt,ind.msg,ind.timestamp)) // FIXME move implicit classes from test to src code so I can do crdt.add
      LocalStep.withIndications(Set(ind),ns)
    }
  }


  def apply[P](all: Set[ProcessId], timeout: Int)(self: ProcessId) = {
    AbstractModule.mod[CRDTMod,CRDTMod#Dep](CRDTState.init(all, timeout)(self),processLocal)
  }


}
