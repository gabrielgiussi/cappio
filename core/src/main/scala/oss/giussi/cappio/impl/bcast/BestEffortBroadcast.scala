package oss.giussi.cappio.impl.bcast

import oss.giussi.cappio.impl.bcast.BestEffortBroadcast.{BEBState, BebBcast, BebDeliver}
import oss.giussi.cappio.impl.net.PerfectLink.{PLDeliver, PLSend, PerfectLinkState}
import oss.giussi.cappio.impl.net.{PerfectLink, Socket}
import oss.giussi.cappio._
import oss.giussi.cappio.impl.bcast.UniformReliableBroadcast.Payload

object BestEffortBroadcast {

  object BebBcast {
    def apply(payload: Any, instance: Instance): BebBcast = new BebBcast(Payload(payload), instance)
  }

  case class BebBcast(payload: Payload, instance: Instance)

  case class BebDeliver(from: ProcessId, payload: Payload)

  object BEBState {
    def init(timeout: Int) = BEBState(PerfectLink.init(timeout))
  }

  case class BEBState(module: Module[PLSend, PerfectLinkState, PLDeliver]) extends StateWithModule[PLSend, PerfectLinkState, PLDeliver, BEBState] {
    override def updateModule(m: Module[PLSend, PerfectLinkState, PLDeliver]): BEBState = copy(m)
  }

  def init(self: ProcessId,all:Set[ProcessId], timeout: Int) = BestEffortBroadcast(self,all,BEBState.init(timeout))
}

case class BestEffortBroadcast(self: ProcessId, all: Set[ProcessId], state: BEBState) extends AbstractModule[BebBcast, BEBState, BebDeliver, PLSend, PerfectLinkState, PLDeliver] {
  override def copyModule(s: BEBState): AbstractModule[BebBcast, BEBState, BebDeliver, PLSend, PerfectLinkState, PLDeliver] = copy(state = s)

  override def processLocal(l: LocalMsg, state: BEBState): LocalStep = l match {
    case Tick => LocalStep(state)
    case PublicRequest(BebBcast(Payload(id,msg), instance)) =>
      val req = all.map(to => LocalRequest(PLSend(Packet(id,msg, self, to, instance))))
      LocalStep.localRequest(req, state)
    case LocalIndication(PLDeliver(Packet(id, payload, from, _, _))) => LocalStep(Set(BebDeliver(from, Payload(id,payload))), state)
  }

  override def t: Socket[PLSend, PerfectLinkState, PLDeliver] = state.module.tail
}