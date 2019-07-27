package oss.giussi.cappio.impl.bcast

import java.util.UUID

import oss.giussi.cappio._
import oss.giussi.cappio.impl.bcast.BestEffortBroadcast.{BebBcast, BebDeliver}
import oss.giussi.cappio.impl.bcast.ReliableBroadcast._
import oss.giussi.cappio.impl.bcast.UniformReliableBroadcast.{DependencyMod, Payload}
import oss.giussi.cappio.impl.time.PerfectFailureDetector
import oss.giussi.cappio.impl.time.PerfectFailureDetector.Crashed
import shapeless.ops.coproduct.Inject
import shapeless.{Inl, Inr}

object ReliableBroadcast {

  val BEB = Instance("beb")
  val PFD = Instance("pfd")

  // puedo hacer esto automatico? por ejemplo con una macro hacer Mod :<>: Mod Y que me de otro Mod con Req = Mod1#Req :+: Mod2#Req

  trait RBMod extends ModS[DependencyMod] {
    override type S = RBcastState
    override type Ind = RBDeliver
    override type Req = RBBcast
  }

  case class RBBcast(payload: Payload) // TODO or Payload? revisar bien este tema a ver si lo estoy haciendo bien

  case class RBDeliver(from: ProcessId, payload: Payload)

  case class RBData(sender: ProcessId, msg: Any)

  object RBcastState {
    def init(self: ProcessId, all: Set[ProcessId], timeout: Int) = {
      val pfdm = PerfectFailureDetector.init(self, all, timeout)
      val bebm = BestEffortBroadcast.init(self, all, timeout)
      RBcastState(all.map(_ -> Set.empty[(UUID, Any)]).toMap, all, CombinedModule.paired(PFD, pfdm, BEB, bebm))
    }
  }

  // TODO use type for (UUID,Any)
  case class RBcastState(delivered: Map[ProcessId, Set[(UUID, Any)]], correct: Set[ProcessId], module: Module[DependencyMod]) extends StateWithModule[DependencyMod, RBcastState] {
    override def updateModule(m: Module[DependencyMod]) = copy(module = m)

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

  // como evitar estos injectors?
  abstract class processLocalHelper2[M <: Mod, Dep <: Mod2](implicit inj1: Inject[Dep#Req, Dep#Dep1#Req], inj2: Inject[Dep#Req, Dep#Dep2#Req]) extends Function2[LocalMsg[M#Req,Dep#Ind],M#State,LocalStep[M#State,M#Ind,Dep#Req,Dep#Ind]] {
    import shapeless.Coproduct

    type Output = LocalStep[M#State,M#Ind,Dep#Req,Dep#Ind]
    type State = M#State

    override def apply(v1: LocalMsg[M#Req,Dep#Ind], state: State): Output = v1 match {
      case PublicRequest(req) => onPublicRequest(req,state)
      case LocalIndication(Inl(ind)) => onDependencyIndication1(ind,state)
      case LocalIndication(Inr(Inl(ind))) => onDependencyIndication2(ind,state)
      case LocalIndication(Inr(Inr(_))) => LocalStep.withState(state)
      case Tick => onTick(state)
    }

    def onPublicRequest(req: M#Req, state: State): Output

    def onDependencyIndication1(ind: Dep#Dep1#Ind, state: State): Output

    def onDependencyIndication2(ind: Dep#Dep2#Ind, state: State): Output

    def onTick(state: State): Output = LocalStep.withState(state)

    def req1(r: Dep#Dep1#Req) : LocalRequest[Dep#Req] = LocalRequest(Coproduct[Dep#Req](r)(inj1))

    // una opcion es que el LocalStep tenga un apply q haga estas conversiones asi no tengo que usar este metodo
    def req2(r: Dep#Dep2#Req): LocalRequest[Dep#Req] = LocalRequest(Coproduct[Dep#Req](r)(inj2))
  }

  def processLocal(self: ProcessId)(implicit inj1: Inject[DependencyMod#Req, DependencyMod#Dep1#Req], inj2: Inject[DependencyMod#Req, DependencyMod#Dep2#Req]): ProcessLocal[RBBcast, RBcastState, RBDeliver, DependencyMod#Req, DependencyMod#Ind] = new processLocalHelper2[RBMod,DependencyMod]{

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

case class ReliableBroadcast(self: ProcessId, state: RBcastState) extends AbstractModule[RBMod,DependencyMod] {
  override def copyModule(s: RBcastState) = copy(state = s)

  override val processLocal = ReliableBroadcast.processLocal(self)
}
