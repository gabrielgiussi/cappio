package oss.giussi.cappio.impl.bcast

import oss.giussi.cappio.Messages.{LocalRequest, LocalStep}
import oss.giussi.cappio.crdt.{VectorTime, Versioned}
import oss.giussi.cappio.impl.bcast.CausalOrderReliableBroadcast.CRBData
import oss.giussi.cappio.{AbstractModule, ModS, Module, Payload, ProcessId, ProcessLocalHelper1, StateWithModule}
import oss.giussi.cappio.impl.bcast.ReliableBroadcast.{RBBcast, RBDeliver, RBMod}

import scala.annotation.tailrec

object WaitingCausalBroadcast {

  type WCBDep[P] = RBMod[Versioned[P]]

  type WCBMod[P] = ModS[WCBDep[P]] {
    type S = WCBState[P]
    type Req = WCBroadcast[P]
    type Ind = WCDeliver[P]
  }

  object WCBroadcast {
    def apply[P](payload: P): WCBroadcast[P] = new WCBroadcast(Payload(payload))
  }

  case class WCBroadcast[P](payload: Payload[P])

  case class WCDeliver[P](msg: P, timestamp: VectorTime)

  case class VersionedFrom[P](processId: ProcessId, versioned: Versioned[P])

  object WCBState {
    def init[P](all: Set[ProcessId], timeout: Int)(self: ProcessId) = WCBState(ReliableBroadcast[Versioned[P]](all, timeout)(self),VectorTime.initial(all.map(_.id.toString)),Set.empty[VersionedFrom[P]])
  }

  case class WCBState[P](module: Module[WCBDep[P]], clock: VectorTime, pending: Set[VersionedFrom[P]]) extends StateWithModule[WCBDep[P], WCBState[P]] {
    override def updateModule(m: Module[WCBDep[P]]): WCBState[P] = copy(module = m)

    def increment(self: ProcessId) = copy(clock = clock.increment(self.id.toString))
  }

  def processLocal[P](self: ProcessId) = new ProcessLocalHelper1[WCBMod[P],WCBDep[P]] {
    override def onPublicRequest(req: WCBroadcast[P], state: State): Output = {
      val WCBroadcast(Payload(id, msg)) = req
      val payload = Payload(id, Versioned(msg, state.clock))
      val bcast = LocalRequest(RBBcast(payload))
      LocalStep.withRequests(Set(bcast), state.increment(self))
    }

    override def onIndication(ind: RBDeliver[Versioned[P]], state: State): Output = {
      val RBDeliver(from, Payload(_, msg)) = ind
      val (deliver,pending,clock) = deliverPending(state.pending + VersionedFrom(from,msg),state.clock)
      LocalStep.withIndications(deliver,state.copy(clock = clock, pending = pending))
    }
  }

  def deliverPending[P](pending: Set[VersionedFrom[P]], initialClock: VectorTime): (Set[WCDeliver[P]],Set[VersionedFrom[P]],VectorTime) = {
    @tailrec
    def dp(delivered: Set[WCDeliver[P]], pending: Set[VersionedFrom[P]], clock: VectorTime): (Set[WCDeliver[P]],Set[VersionedFrom[P]],VectorTime) = {
      pending.find(_.versioned.vectorTimestamp <= clock) match {
        case Some(vf@VersionedFrom(p,v)) => dp(delivered + WCDeliver(v.value,v.vectorTimestamp),pending - vf, clock.increment(p.id.toString)) // TODO clock.merge(v.vectorTimestamp)
        case None => (delivered,pending,clock)
      }
    }
    dp(Set.empty,pending,initialClock)
  }

  def apply[P](all: Set[ProcessId], timeout: Int)(self: ProcessId): Module[WCBMod[P]] = AbstractModule.mod[WCBMod[P],WCBMod[P]#Dep](WCBState.init(all,timeout)(self),processLocal(self))

}
