package oss.giussi.cappio.impl.bcast

import java.util.UUID

import oss.giussi.cappio._
import oss.giussi.cappio.impl.bcast.BestEffortBroadcast.{BebBcast, BebDeliver, BebMod}
import oss.giussi.cappio.impl.bcast.ReliableBroadcast._
import oss.giussi.cappio.impl.time.PerfectFailureDetector
import oss.giussi.cappio.impl.time.PerfectFailureDetector.{Crashed, PFDMod}
import shapeless.ops.coproduct.Inject
import shapeless.{Inl, Inr}

object ReliableBroadcast {
/*
  val BEB = Instance("beb")
  val PFD = Instance("pfd")

  // puedo hacer esto automatico? por ejemplo con una macro hacer Mod :<>: Mod Y que me de otro Mod con Req = Mod1#Req :+: Mod2#Req

  type RBDep[P] = Mod2 {
    type Dep1 = PFDMod
    type Dep2 = BebMod[RBData[P]]
    type State = (Dep1#State, Dep2#State)
  }

  trait RBMod[P] extends ModS[RBDep[P],P] {
    override type S = RBcastState[P]
    override type Ind = RBDeliver[P]
    override type Req = RBBcast[P]
  }

  case class RBBcast[P](payload: Payload[P]) // TODO or Payload? revisar bien este tema a ver si lo estoy haciendo bien

  case class RBDeliver[P](from: ProcessId, payload: Payload[P])

  case class RBData[P](sender: ProcessId, msg: P)

  object RBcastState {
    def init(self: ProcessId, all: Set[ProcessId], timeout: Int) = {
      val pfdm = PerfectFailureDetector.init(self, all, timeout)
      val bebm = BestEffortBroadcast.init(self, all, timeout)
      RBcastState(all.map(_ -> Set.empty[(UUID, Any)]).toMap, all, CombinedModule.paired(PFD, pfdm, BEB, bebm))
    }
  }

  // TODO use type for (UUID,Any)
  case class RBcastState[P](delivered: Map[ProcessId, Set[(UUID, Any)]], correct: Set[ProcessId], module: Module[URBDep]) extends StateWithModule[URBDep, RBcastState] {
    override def updateModule(m: Module[URBDep]) = copy(module = m)

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

  def init(self: ProcessId, all: Set[ProcessId], timeout: Int) = ReliableBroadcast(self, RBcastState.init(self, all, timeout))

  import oss.giussi.cappio.Messages._

  trait processLocalHelper[M <: Mod, Dep <: Mod] extends Function2[LocalMsg[M#Req,Dep#Ind],M#State,LocalStep[M#State,M#Ind,Dep#Req,Dep#Ind]] {

  }

  def processLocal(self: ProcessId)(implicit inj1: Inject[URBDep#Req, URBDep#Dep1#Req], inj2: Inject[URBDep#Req, URBDep#Dep2#Req]): ProcessLocal[RBBcast, RBcastState, RBDeliver, URBDep#Req, URBDep#Ind] = new processLocalHelper2[RBMod,URBDep]{

    override def onPublicRequest(req: RBBcast, state: State): Output = LocalStep.withRequests(Set(req2(BebBcast(Payload(req.payload.id, RBData(self, req.payload.msg)), ReliableBroadcast.BEB))), state)

    override def onDependencyIndication1(ind: Crashed, state: State): Output = {
      val (ns, toBcast) = state.crashed(ind.id)
      val requests = toBcast.map { case (uuid, msg) => req2(BebBcast(Payload(uuid, RBData(ind.id, msg)), ReliableBroadcast.BEB)) }
      LocalStep.withRequests(requests, ns)
    }

    override def onDependencyIndication2(ind: BebDeliver, state: State): Output = {
      val BebDeliver(_, Payload(id, RBData(sender, msg))) = ind
      state.deliver(sender, id, msg) match {
        case Some((ns, bcast)) =>
          val ind = Set(RBDeliver(sender, Payload(id, msg)))
          val req = bcast.map { case (uuid, p) => req2(BebBcast(Payload(uuid, p), ReliableBroadcast.BEB)) } // TODO el p lo tengo que poner en un RBData?
          LocalStep.withRequestsAndIndications(ind, req, ns)
        case None => LocalStep.withState(state)
      }
    }
  }
}

case class ReliableBroadcast(self: ProcessId, state: RBcastState) extends AbstractModule[RBMod,URBDep] {
  override def copyModule(s: RBcastState) = copy(state = s)

  override val processLocal = ReliableBroadcast.processLocal(self)

 */
}
