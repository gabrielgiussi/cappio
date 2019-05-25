package oss.giussi.cappio.impl.bcast

import java.util.UUID

import oss.giussi.cappio.impl.bcast.CausalOrderReliableBroadcast.{CRBBroadcast, CRBData, CRBDeliver, CRBState}
import oss.giussi.cappio.impl.bcast.ReliableBroadcast.{RBBcast, RBDeliver, RBcastState}
import oss.giussi.cappio.impl.bcast.UniformReliableBroadcast.Payload
import oss.giussi.cappio.{AbstractModule, Module, ProcessId, StateWithModule}

object CausalOrderReliableBroadcast {
  case class CRBBroadcast(payload: Payload)
  case class CRBDeliver(from: ProcessId, msg: Any)

  case class CRBData(mpast: Past, msg: Any)

  object Past {
    def apply(): Past = new Past(List())
  }

  case class Past(list: List[(ProcessId,UUID,Any)]) {
    def appendIfNotExists(sender: ProcessId, id: UUID, msg: Any): Past = copy(list :+ (sender,id,msg))
  }

  object CRBState {

    def init(self: ProcessId, all: Set[ProcessId], timeout: Int) = CRBState(Past(),Set.empty,ReliableBroadcast.init(self,all,timeout))
  }

  case class CRBState(past: Past,delivered: Set[UUID],module: Module[RBBcast,RBcastState,RBDeliver]) extends StateWithModule[RBBcast,RBcastState,RBDeliver,CRBState] {
    override def updateModule(m: Module[RBBcast, RBcastState, RBDeliver]): CRBState = copy(module = m)

    def bcast(self: ProcessId, id: UUID, msg: Any) = copy(past = past.appendIfNotExists(self,id,msg))

    def deliver(sender: ProcessId, mpast: Past, id: UUID, msg: Any) = {
      if (delivered contains(id)) None
      else {
        val undelivered = mpast.list.filterNot { case (_,id,_) => delivered.contains(id) }
        val nPast = undelivered.foldLeft(past){ case (f,(s,id,m)) => f.appendIfNotExists(s,id,m) }
        val toDeliver = undelivered :+ (sender,id,msg)
        Some((copy(delivered = delivered ++ toDeliver.map(_._2), past = nPast)),toDeliver)
      }
    }
  }

  def init(self: ProcessId, all: Set[ProcessId], timeout: Int) = CausalOrderReliableBroadcast(self,CRBState.init(self,all,timeout))
}

case class CausalOrderReliableBroadcast(self: ProcessId, state: CRBState) extends AbstractModule[CRBBroadcast,CRBState,CRBDeliver,RBBcast,RBcastState,RBDeliver] {
  override def copyModule(s: CRBState): AbstractModule[CRBBroadcast, CRBState, CRBDeliver, RBBcast, RBcastState, RBDeliver] = copy(state = s)

  override def processLocal(l: LocalMsg, state: CRBState): LocalStep = l match {
    case Tick => LocalStep(state)
    case PublicRequest(CRBBroadcast(Payload(id,msg))) =>
      val payload = Payload(id,CRBData(state.past,msg))
      val bcast = LocalRequest(RBBcast(payload))
      LocalStep.localRequest(Set(bcast),state.bcast(self,payload.id,msg))
    case LocalIndication(RBDeliver(from,Payload(id,CRBData(mpast,msg)))) =>
      state.deliver(from,mpast,id,msg) match {
        case Some((ns,toDeliver)) =>
          val ind = toDeliver.map { case (sender,_,m) => CRBDeliver(sender,m) }.toSet
          LocalStep(ind,ns)
        case None => LocalStep(state)
      }


  }

}
