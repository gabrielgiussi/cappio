package oss.giussi.cappio.impl.bcast

import java.util.UUID

import oss.giussi.cappio.Messages.{LocalRequest, LocalStep}
import oss.giussi.cappio._
import oss.giussi.cappio.impl.AppState
import oss.giussi.cappio.impl.AppState.AppMod2
import oss.giussi.cappio.impl.bcast.ReliableBroadcast.{RBBcast, RBDeliver, RBMod}

object CausalOrderReliableBroadcast {

  object CRBBroadcast {
    def apply[P](msg: P): CRBBroadcast[P] = new CRBBroadcast(Payload(msg))
  }

  case class CRBBroadcast[P](payload: Payload[P])

  case class CRBDeliver[P](from: ProcessId, msg: P) // TODO should be Payload?

  case class CRBData[P](mpast: Past[P], msg: P)

  object Past {
    def empty[P](): Past[P] = new Past(List.empty[(ProcessId, UUID, P)])
  }

  case class Past[P](list: List[(ProcessId, UUID, P)]) {
    def appendIfNotExists(sender: ProcessId, id: UUID, msg: P): Past[P] = copy(list :+ (sender, id, msg))
  }

  type CORBDep[P] = RBMod[CRBData[P]]

  type CORBMod[P] = ModS[CORBDep[P]] {
    type S = CRBState[P]
    type Req = CRBBroadcast[P]
    type Ind = CRBDeliver[P]
  }

  type CausalApp[P] = AppMod2[P,CORBMod[P]]

  object CRBState {

    def init[P](all: Set[ProcessId], timeout: Int)(self: ProcessId) = CRBState(Past.empty[P], Set.empty, ReliableBroadcast[CRBData[P]](all, timeout)(self))
  }

  case class CRBState[P](past: Past[P], delivered: Set[UUID], module: Module[CORBDep[P]]) extends StateWithModule[CORBDep[P], CRBState[P]] {
    override def updateModule(m: Module[CORBDep[P]]) = copy(module = m)

    def bcast(self: ProcessId, id: UUID, msg: P) = copy(past = past.appendIfNotExists(self, id, msg))

    def deliver(sender: ProcessId, mpast: Past[P], id: UUID, msg: P) = {
      if (delivered contains id) None
      else {
        val undelivered = mpast.list.filterNot { case (_, id, _) => delivered.contains(id) }
        val nPast = undelivered.foldLeft(past) { case (f, (s, id, m)) => f.appendIfNotExists(s, id, m) }
        val toDeliver = undelivered :+ (sender, id, msg)
        Some((copy(delivered = delivered ++ toDeliver.map(_._2), past = nPast)), toDeliver)
      }
    }
  }

  def apply[P](all: Set[ProcessId], timeout: Int)(self: ProcessId) = {
    AbstractModule.mod[CORBMod[P],CORBMod[P]#Dep](CRBState.init[P](all, timeout)(self),processLocal[P](self))
  }

  def app[P](all: Set[ProcessId], timeout: Int)(self: ProcessId) = {
    val cb = CausalOrderReliableBroadcast[P](all,timeout)(self)
    AppState.app2[P,CORBMod[P]](cb,(state,ind) => state.update(ind.msg))
  }

  def processLocal[P](self: ProcessId) = new ProcessLocalHelper1[CORBMod[P],CORBDep[P]] {
    override def onPublicRequest(req: CRBBroadcast[P], state: State): Output = {
      val CRBBroadcast(Payload(id, msg)) = req
      val payload = Payload(id, CRBData(state.past, msg))
      val bcast = LocalRequest(RBBcast(payload))
      LocalStep.withRequests(Set(bcast), state.bcast(self, payload.id, msg))
    }

    override def onIndication(ind: RBDeliver[CRBData[P]], state: State): Output = {
      val RBDeliver(from, Payload(id, CRBData(mpast, msg))) = ind
      state.deliver(from, mpast, id, msg) match {
        case Some((ns, toDeliver)) =>
          val ind = toDeliver.map { case (sender, _, m) => CRBDeliver(sender, m) }.toSet
          LocalStep.withIndications(ind, ns)
        case None => LocalStep.withState(state)
      }
    }
  }

}

