package oss.giussi.cappio.impl.bcast

import java.util.UUID

import oss.giussi.cappio.{AbstractModule, CombinedModule, Instance, Module, NoRequest, ProcessId, StateWithModule}
import oss.giussi.cappio.impl.bcast.BestEffortBroadcast.{BEBState, BebBcast, BebDeliver}
import oss.giussi.cappio.impl.bcast.ReliableBroadcast.{ModuleInd, ModuleReq, ModuleState, RBBcast, RBData, RBDeliver, RBcastState}
import oss.giussi.cappio.impl.bcast.UniformReliableBroadcast.Payload
import oss.giussi.cappio.impl.time.PerfectFailureDetector
import oss.giussi.cappio.impl.time.PerfectFailureDetector.{Crashed, PFDState}

object ReliableBroadcast {

  val BEB = Instance("beb")
  val PFD = Instance("pfd")

  type ModuleReq = Either[NoRequest, BebBcast]

  type ModuleState = (PFDState, BEBState)

  type ModuleInd = Either[Crashed, BebDeliver]

  case class RBBcast(payload: Payload) // TODO or Payload? revisar bien este tema a ver si lo estoy haciendo bien

  case class RBDeliver(from: ProcessId, payload: Payload)

  case class RBData(sender: ProcessId, msg: Any)

  object RBcastState {
    def init(self: ProcessId, all: Set[ProcessId], timeout: Int) = {
      val pfdm: PerfectFailureDetector = PerfectFailureDetector.init(self, all, timeout)
      val bebm = BestEffortBroadcast.init(self, all, timeout)
      val s = (s1: PFDState, s2: BEBState) => (s1, s2)
      RBcastState(all.map(_ -> Set.empty[(UUID,Any)]).toMap, all, CombinedModule(PFD, pfdm, BEB, bebm, s))
    }
  }

  // TODO use type for (UUID,Any)
  case class RBcastState(delivered: Map[ProcessId, Set[(UUID, Any)]], correct: Set[ProcessId], module: Module[ModuleReq, ModuleState, ModuleInd]) extends StateWithModule[ModuleReq, ModuleState, ModuleInd, RBcastState] {
    override def updateModule(m: Module[ModuleReq, (PFDState, BEBState), ModuleInd]): RBcastState = copy(module = m)

    def crashed(id: ProcessId): (RBcastState, Set[(UUID, Any)]) = (copy(correct = correct - id), delivered(id))

    def deliver(sender: ProcessId, id: UUID, msg: Any) = {
      if (delivered(sender).contains((id, msg))) None
      else {
        val ns = copy(delivered = delivered.updated(sender, delivered(sender) + (id -> msg)))
        val toBcast = if (correct.contains(sender)) Set.empty else Set((id, msg))
        Some((ns, toBcast))
      }
    }
  }

  def init(self: ProcessId, all: Set[ProcessId], timeout: Int) = ReliableBroadcast(self,RBcastState.init(self,all,timeout))

}

case class ReliableBroadcast(self: ProcessId, state: RBcastState) extends AbstractModule[RBBcast, RBcastState, RBDeliver, ModuleReq, ModuleState, ModuleInd] {
  override def copyModule(s: RBcastState): AbstractModule[RBBcast, RBcastState, RBDeliver, ModuleReq, (PFDState, BEBState), ModuleInd] = copy(state = s)

  override def processLocal(l: LocalMsg, state: RBcastState): LocalStep = l match {
    case Tick => LocalStep(state)
    case PublicRequest(RBBcast(p)) => LocalStep.localRequest(Set(LocalRequest(Right(BebBcast(Payload(p.id,RBData(self,p.msg)), ReliableBroadcast.BEB)))), state)
    case LocalIndication(Left(Crashed(id))) =>
      val (ns, toBcast) = state.crashed(id)
      val requests = toBcast.map { case (uuid,msg) => LocalRequest(Right(BebBcast(Payload(uuid,RBData(id, msg)), ReliableBroadcast.BEB))) }
      LocalStep.localRequest(requests, ns)
    case LocalIndication(Right(BebDeliver(_, Payload(id, RBData(sender, msg))))) =>
      state.deliver(sender, id, msg) match {
        case Some((ns, bcast)) =>
          val ind = Set(RBDeliver(sender,Payload(id,msg)))
          val req = bcast.map { case (uuid,p) => LocalRequest(Right(BebBcast(Payload(uuid,p), ReliableBroadcast.BEB))) } // TODO el p lo tengo que poner en un RBData?
          LocalStep.prueba(ind,req, ns)
        case None => LocalStep(state)
      }
  }

}
