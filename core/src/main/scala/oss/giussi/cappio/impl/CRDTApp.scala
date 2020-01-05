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
    type S = AWSet[String]
    type Req = SetRequest
    type Ind = WCDeliver[SetOp]
  }

  sealed trait SetRequest

  case class Add(payload: String) extends SetRequest

  case class Remove(payload: String) extends SetRequest


  object CRDTState {
    def init(all: Set[ProcessId], timeout: Int)(self: ProcessId) = StateWithModule(WaitingCausalBroadcast[SetOp](all,timeout)(self), AWSetService.zero[String])
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
      val ns = state.updateState(crdt => ops.effect(crdt,ind.msg,ind.timestamp)) // FIXME move implicit classes from test to src code so I can do crdt.add
      LocalStep.withIndications(Set(ind),ns)
    }
  }


  def apply[P](all: Set[ProcessId], timeout: Int)(self: ProcessId): Module[CRDTMod] = {
    AbstractModule.mod[CRDTMod,CRDTMod#Dep](CRDTState.init(all, timeout)(self),processLocal)
  }


}
