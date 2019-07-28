package oss.giussi.cappio.impl.bcast

import oss.giussi.cappio.Messages.ProcessLocal
import oss.giussi.cappio._
import oss.giussi.cappio.impl.bcast.BestEffortBroadcast.{BEBState, BebMod}
import oss.giussi.cappio.impl.net.PerfectLink.{PLDeliver, PLSend}
import oss.giussi.cappio.impl.net.PerfectLinkBeta
import oss.giussi.cappio.impl.net.PerfectLinkBeta.PLModule

object BestEffortBroadcast {

  type BebMod[P] = ModS[PLModule[P],P] {
    type Req = BebBcast[P]
    type S = BEBState[P]
    type Ind = BebDeliver[P]
  }

  case class BebBcast[P](payload: Payload[P], instance: Instance)

  case class BebDeliver[P](from: ProcessId, payload: Payload[P])

  object BEBState {
    def init[P](timeout: Int) = BEBState(PerfectLinkBeta.init[P](timeout))
  }

  case class BEBState[P](module: Module[BebMod[P]#Dep]) extends StateWithModule[BebMod[P]#Dep, BEBState[P]] {
    override def updateModule(m: Module[BebMod[P]#Dep]): BEBState[P] = copy(m)
  }

  def init[P](self: ProcessId,all:Set[ProcessId], timeout: Int): BestEffortBroadcast[P] = BestEffortBroadcast(self,all,BEBState.init[P](timeout))

  def processLocal[P](self: ProcessId, all: Set[ProcessId]): ProcessLocal[BebBcast[P], BEBState[P], BebDeliver[P], PLSend[P], PLDeliver[P],P] = {
    import Messages._
    (msg,state) => msg match {
      case Tick => LocalStep.withState(state)
      case PublicRequest(BebBcast(Payload(id,msg), instance)) =>
        val req = all.map(to => LocalRequest(PLSend(Packet(id,msg, self, to, instance))))
        LocalStep.withRequests(req, state)
      case LocalIndication(PLDeliver(Packet(id, payload, from, _, _))) => LocalStep.withIndications(Set(BebDeliver(from, Payload(id,payload))), state)
    }
  }
}

case class BestEffortBroadcast[P](self: ProcessId, all: Set[ProcessId], state: BEBState[P]) extends AbstractModule[BebMod[P],BebMod[P]#Dep,P] {
  override def copyModule(s: BEBState[P]) = copy(state = s)

  override val processLocal: PLocal = BestEffortBroadcast.processLocal(self,all)
}