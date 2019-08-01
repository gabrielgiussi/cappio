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

  object BebBcast {
    def apply[P](msg: P): BebBcast[P] = new BebBcast(Payload(msg), Instance.ANY)
  }

  case class BebBcast[P](payload: Payload[P], instance: Instance) {
    // TODO move to a typeclass in the ui
    override def toString: String = s"beb ${payload.msg}"
  }

  case class BebDeliver[P](from: ProcessId, payload: Payload[P])

  object BEBState {
    def init[P](timeout: Int) = BasicState(PerfectLinkBeta.init[P](timeout))
  }


  def init[P](all:Set[ProcessId], timeout: Int)(self: ProcessId): BestEffortBroadcast[P] = BestEffortBroadcast(self,all,BEBState.init[P](timeout))

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

  def apply[T](self: ProcessId, all: Set[ProcessId], timeout: Int): Module[BebMod[T]] = {
    AbstractModule.mod[BebMod[T],BebMod[T]#Dep](BasicState(PerfectLinkBeta.init[T](timeout)),BestEffortBroadcast.processLocal[T](self,all))
  }

}

case class BestEffortBroadcast[T](self: ProcessId, all: Set[ProcessId], state: BasicState[BebMod[T]#Dep]) extends AbstractModule[BebMod[T],BebMod[T]#Dep] {
  override def copyModule(s: BasicState[BebMod[T]#Dep]) = copy(state = s)

  override val processLocal: PLocal = BestEffortBroadcast.processLocal(self,all)
}