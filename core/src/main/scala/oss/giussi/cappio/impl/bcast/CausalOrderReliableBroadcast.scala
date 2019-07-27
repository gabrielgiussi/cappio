package oss.giussi.cappio.impl.bcast

import java.util.UUID

import oss.giussi.cappio.Messages.ProcessLocal
import oss.giussi.cappio.impl.bcast.CausalOrderReliableBroadcast.{CORBMod, CRBBroadcast, CRBDeliver, CRBState}
import oss.giussi.cappio.impl.bcast.ReliableBroadcast.{RBBcast, RBDeliver, RBMod, RBcastState}
import oss.giussi.cappio.impl.bcast.UniformReliableBroadcast.Payload
import oss.giussi.cappio.{AbstractModule, ModS, Module, ProcessId, StateWithModule}

object CausalOrderReliableBroadcast {

  case class CRBBroadcast(payload: Payload)

  case class CRBDeliver(from: ProcessId, msg: Any)

  case class CRBData(mpast: Past, msg: Any)

  object Past {
    def apply(): Past = new Past(List())
  }

  case class Past(list: List[(ProcessId, UUID, Any)]) {
    def appendIfNotExists(sender: ProcessId, id: UUID, msg: Any): Past = copy(list :+ (sender, id, msg))
  }

  type CORBMod = ModS[RBMod] {
    type S = CRBState
    type Req = CRBBroadcast
    type Ind = CRBDeliver
  }

  object CRBState {

    def init(self: ProcessId, all: Set[ProcessId], timeout: Int) = CRBState(Past(), Set.empty, ReliableBroadcast.init(self, all, timeout))
  }

  case class CRBState(past: Past, delivered: Set[UUID], module: Module[RBMod]) extends StateWithModule[RBMod, CRBState] {
    override def updateModule(m: Module[RBMod]): CRBState = copy(module = m)

    def bcast(self: ProcessId, id: UUID, msg: Any) = copy(past = past.appendIfNotExists(self, id, msg))

    def deliver(sender: ProcessId, mpast: Past, id: UUID, msg: Any) = {
      if (delivered contains id) None
      else {
        val undelivered = mpast.list.filterNot { case (_, id, _) => delivered.contains(id) }
        val nPast = undelivered.foldLeft(past) { case (f, (s, id, m)) => f.appendIfNotExists(s, id, m) }
        val toDeliver = undelivered :+ (sender, id, msg)
        Some((copy(delivered = delivered ++ toDeliver.map(_._2), past = nPast)), toDeliver)
      }
    }
  }

  def init(self: ProcessId, all: Set[ProcessId], timeout: Int) = CausalOrderReliableBroadcast(self, CRBState.init(self, all, timeout))

  def processLocal(self: ProcessId): ProcessLocal[CRBBroadcast, CRBState, CRBDeliver, RBBcast, RBDeliver] = {
    import oss.giussi.cappio.Messages._
    (msg, state) =>
      msg match {
        case PublicRequest(CRBBroadcast(Payload(id, msg))) =>
          val payload = Payload(id, CRBData(state.past, msg))
          val bcast = LocalRequest(RBBcast(payload))
          LocalStep.withRequests(Set(bcast), state.bcast(self, payload.id, msg))
        case LocalIndication(RBDeliver(from, Payload(id, CRBData(mpast, msg)))) =>
          state.deliver(from, mpast, id, msg) match {
            case Some((ns, toDeliver)) =>
              val ind = toDeliver.map { case (sender, _, m) => CRBDeliver(sender, m) }.toSet
              LocalStep.withIndications(ind, ns)
            case None => LocalStep.withState(state)
          }
        case _ => LocalStep.withState(state)
      }
  }
}

case class CausalOrderReliableBroadcast(self: ProcessId, state: CRBState) extends AbstractModule[CORBMod, RBMod] {
  override def copyModule(s: CRBState) = copy(state = s)

  override val processLocal: PLocal = CausalOrderReliableBroadcast.processLocal(self)

}
