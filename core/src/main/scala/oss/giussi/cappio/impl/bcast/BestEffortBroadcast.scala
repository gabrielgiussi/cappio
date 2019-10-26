package oss.giussi.cappio.impl.bcast

import oss.giussi.cappio.Messages.{LocalRequest, LocalStep}
import oss.giussi.cappio._
import oss.giussi.cappio.impl.AppState
import oss.giussi.cappio.impl.AppState.AppMod2
import oss.giussi.cappio.impl.net.PerfectLink
import oss.giussi.cappio.impl.net.PerfectLink.{PLDeliver, PLModule, PLSend}

object BestEffortBroadcast {

  type BebMod[P] = ModS[PLModule[P]] {
    type Req = BebBcast[P] // TODO shouldn't be BebBcast[PLModule#Payload] ?
    type S = BasicState[PLModule[P]]
    type Ind = BebDeliver[P]
  }

  object BebBcast {
    def apply[P](msg: P): BebBcast[P] = new BebBcast(Payload(msg), Instance.ANY)
  }

  case class BebBcast[P](payload: Payload[P], instance: Instance)

  case class BebDeliver[P](from: ProcessId, payload: Payload[P])

  object BEBState {
    def init[P](timeout: Int) = BasicState(PerfectLink.init[P](timeout))
  }

  def processLocal[P](self: ProcessId, all: Set[ProcessId]) = new ProcessLocalHelper1[BebMod[P],PLModule[P]] {
    override def onPublicRequest(req: BebBcast[P], state: State): Output = {
      val BebBcast(Payload(id,msg), instance) = req
      val reqs = all.map(to => LocalRequest(PLSend(Packet(id,msg, self, to, instance))))
      LocalStep.withRequests(reqs, state)
    }

    override def onIndication(ind: DInd, state: State): Output = {
      val PLDeliver(Packet(id, payload, from, _, _)) = ind
      LocalStep.withIndications(Set(BebDeliver(from, Payload(id,payload))), state)
    }

  }

  def apply[T](all: Set[ProcessId], timeout: Int)(self: ProcessId): Module[BebMod[T]] = {
    AbstractModule.mod[BebMod[T],BebMod[T]#Dep](BasicState(PerfectLink.init[T](timeout)),BestEffortBroadcast.processLocal[T](self,all))
  }

  type BebApp[P] = AppMod2[P, BebMod[P]]

  def app[P](all: Set[ProcessId], timeout: Int)(self: ProcessId): Module[BebApp[P]] = {
    val beb = BestEffortBroadcast[P](all,timeout)(self)
    AppState.app2[P,BebMod[P]](beb,(state,ind) => state.update(ind.payload.msg))
  }

}