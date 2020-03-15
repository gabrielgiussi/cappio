package oss.giussi.cappio.impl.bcast

import oss.giussi.cappio.Messages.{LocalRequest, LocalStep}
import oss.giussi.cappio.crdt.{VectorTime, Versioned}
import oss.giussi.cappio.impl.bcast.CausalOrderReliableBroadcast.CRBData
import oss.giussi.cappio.impl.bcast.EagerReliableBroadcast.ERBMod
import oss.giussi.cappio.{AbstractModule, ModS, Module, Payload, ProcessId, ProcessLocalHelper1, StateWithModule}
import oss.giussi.cappio.impl.bcast.ReliableBroadcast.{RBBcast, RBDeliver, RBMod}

import scala.annotation.tailrec

object WaitingCausalBroadcast {

  type WCBDep[P] = ERBMod[Versioned[P]]

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
    def init[P](all: Set[ProcessId], timeout: Int)(self: ProcessId) = StateWithModule(EagerReliableBroadcast[Versioned[P]](all, timeout)(self),WCBState(VectorTime.initial(all.map(_.id.toString)),0,Set.empty[VersionedFrom[P]]))
  }

  case class WCBState[P](clock: VectorTime, lsn: Integer, pending: Set[VersionedFrom[P]]) {

    def increment(self: ProcessId) = copy(clock = clock.increment(self.id.toString))
  }

  def processLocal[P](self: ProcessId) = new ProcessLocalHelper1[WCBMod[P],WCBDep[P]] {
    override def onPublicRequest(req: WCBroadcast[P], state: State): Output = {
      val WCBroadcast(Payload(id, msg)) = req
      val W = VectorTime(state.state.clock.value.updated(self.id.toString,state.state.lsn.toLong))
      val payload = Payload(id, Versioned(msg, W))
      val bcast = LocalRequest(RBBcast(payload))
      LocalStep.withRequests(Set(bcast), state.updateState(s => s.copy(lsn = s.lsn + 1)))
    }

    override def onIndication(ind: RBDeliver[Versioned[P]], state: State): Output = {
      val RBDeliver(from, Payload(_, msg)) = ind
      val (deliver,pending,clock) = deliverPending(state.state.pending + VersionedFrom(from,msg),state.state.clock)
      LocalStep.withIndications(deliver,state.updateState(_.copy(clock = clock, pending = pending)))
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
