package oss.giussi.cappio.impl.bcast

import oss.giussi.cappio.Messages.{LocalRequest, LocalStep, ProcessLocalM}
import oss.giussi.cappio._
import oss.giussi.cappio.impl.bcast.BestEffortBroadcast.BebMod
import oss.giussi.cappio.impl.net.PerfectLink.{PLDeliver, PLSend}
import oss.giussi.cappio.impl.net.PerfectLinkBeta
import oss.giussi.cappio.impl.net.PerfectLinkBeta.PLModule

object BestEffortBroadcast {

  type BebMod[P] = ModS[PLModule[P]] {
    type Req = BebBcast[P]
    type S = BasicState[PLModule[P]]
    type Ind = BebDeliver[P]
  }

  case class BebBcast[P](payload: Payload[P], instance: Instance)

  case class BebDeliver[P](from: ProcessId, payload: Payload[P])

  object BEBState {
    def init[P](timeout: Int) = BasicState(PerfectLinkBeta.init[P](timeout))
  }


  def init[P](self: ProcessId,all:Set[ProcessId], timeout: Int): BestEffortBroadcast[P] = BestEffortBroadcast(self,all,BEBState.init[P](timeout))

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

  def beb[T](self: ProcessId, all: Set[ProcessId]): Module[BebMod[T]] = {
    AbstractModule.mod[BebMod[T],BebMod[T]#Dep](BasicState(PerfectLinkBeta.init[T](4)),BestEffortBroadcast.processLocal[T](self,all))
  }

}

case class BestEffortBroadcast[T](self: ProcessId, all: Set[ProcessId], state: BasicState[BebMod[T]#Dep]) extends AbstractModule[BebMod[T],BebMod[T]#Dep] {
  override def copyModule(s: BasicState[BebMod[T]#Dep]) = copy(state = s)

  override val processLocal: PLocal = BestEffortBroadcast.processLocal(self,all)
}