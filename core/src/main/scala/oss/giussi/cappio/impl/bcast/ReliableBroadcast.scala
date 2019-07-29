package oss.giussi.cappio.impl.bcast

import java.util.UUID

import oss.giussi.cappio._
import oss.giussi.cappio.impl.bcast.BestEffortBroadcast.{BebBcast, BebDeliver, BebMod}
import oss.giussi.cappio.impl.bcast.ReliableBroadcast._
import oss.giussi.cappio.impl.time.PerfectFailureDetector
import oss.giussi.cappio.impl.time.PerfectFailureDetector.{Crashed, PFDMod}
import shapeless.ops.coproduct.Inject

object ReliableBroadcast {
  val BEB = Instance("beb")
  val PFD = Instance("pfd")

  type RBDep[P] = Mod2 {
    type Dep1 = PFDMod
    type Dep2 = BebMod[RBData[P]]
    type State = (Dep1#State, Dep2#State)
  }

  trait RBMod[P] extends ModS[RBDep[P]] {
    override type S = RBcastState[P]
    override type Ind = RBDeliver[P]
    override type Req = RBBcast[P]
  }

  case class RBBcast[P](payload: Payload[P]) // TODO or Payload? revisar bien este tema a ver si lo estoy haciendo bien

  case class RBDeliver[P](from: ProcessId, payload: Payload[P])

  case class RBData[P](sender: ProcessId, msg: P)

  object RBcastState {
    def init[P](self: ProcessId, all: Set[ProcessId], timeout: Int) = {
      val pfdm = PerfectFailureDetector.init(self, all, timeout)
      val bebm = BestEffortBroadcast.init[RBData[P]](self, all, timeout)
      RBcastState(all.map(_ -> Set.empty[(UUID, P)]).toMap, all, CombinedModule.paired(PFD, pfdm, BEB, bebm))
    }
  }

  // TODO use type for (UUID,Any)
  case class RBcastState[P](delivered: Map[ProcessId, Set[(UUID, P)]], correct: Set[ProcessId], module: Module[RBDep[P]]) extends StateWithModule[RBDep[P], RBcastState[P]] {
    override def updateModule(m: Module[RBDep[P]]) = copy(module = m)

    def crashed(id: ProcessId): (RBcastState[P], Set[(UUID, P)]) = (copy(correct = correct - id), delivered(id))

    def deliver(sender: ProcessId, id: UUID, msg: P) = {
      if (delivered(sender).contains((id, msg))) None
      else {
        val ns = copy(delivered = delivered.updated(sender, delivered(sender) + (id -> msg)))
        val toBcast = if (correct.contains(sender)) Set.empty else Set((id, msg))
        Some((ns, toBcast))
      }
    }
  }

  def init[P](self: ProcessId, all: Set[ProcessId], timeout: Int) = ReliableBroadcast(self, RBcastState.init[P](self, all, timeout))

  import oss.giussi.cappio.Messages._

  def processLocal[P](self: ProcessId)(implicit inj1: Inject[RBDep[P]#Req, RBDep[P]#Dep1#Req], inj2: Inject[RBDep[P]#Req, RBDep[P]#Dep2#Req]) = new processLocalHelper2[RBMod[P],RBDep[P]]{

    override def onPublicRequest(req: RBBcast[P], state: State): Output = LocalStep.withRequests(Set(req2(BebBcast(Payload(req.payload.id, RBData(self, req.payload.msg)), ReliableBroadcast.BEB))), state)

    override def onDependencyIndication1(ind: Crashed, state: State): Output = {
      val (ns, toBcast) = state.crashed(ind.id)
      val requests = toBcast.map { case (uuid, msg) => req2(BebBcast(Payload(uuid, RBData(ind.id, msg)), ReliableBroadcast.BEB)) }
      LocalStep.withRequests(requests, ns)
    }

    override def onDependencyIndication2(ind: BebDeliver[RBData[P]], state: State): Output = {
      val BebDeliver(_, Payload(id, RBData(sender, msg))) = ind
      state.deliver(sender, id, msg) match {
        case Some((ns, bcast)) =>
          val ind = Set(RBDeliver(sender, Payload(id, msg)))
          val req = bcast.map { case (uuid, p) => req2(BebBcast(Payload(uuid, RBData(self,p)), ReliableBroadcast.BEB)) }
          LocalStep.withRequestsAndIndications(ind, req, ns)
        case None => LocalStep.withState(state)
      }
    }
  }
}

case class ReliableBroadcast[T](self: ProcessId, state: RBcastState[T]) extends AbstractModule[RBMod[T],RBDep[T]] {
  override def copyModule(s: RBcastState[T]) = copy(state = s)

  override val processLocal = ReliableBroadcast.processLocal(self)

}
