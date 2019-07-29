package oss.giussi.cappio.impl.bcast

import java.util.UUID

import oss.giussi.cappio.Messages.{LocalStep, ProcessLocal}
import oss.giussi.cappio._
import oss.giussi.cappio.impl.bcast.BestEffortBroadcast.{BebBcast, BebDeliver, BebMod}
import oss.giussi.cappio.impl.bcast.UniformReliableBroadcast.{URBDep, URBMod, URBState}
import oss.giussi.cappio.impl.time.PerfectFailureDetector
import oss.giussi.cappio.impl.time.PerfectFailureDetector.{Crashed, PFDMod}
import shapeless.ops.coproduct.Inject

import scala.util.Random


object UniformReliableBroadcast {

  type Acks = Map[UUID, Set[ProcessId]]


  type URBDep[P] = Mod2 {
    type Dep1 = PFDMod
    type Dep2 = BebMod[URBData[P]]
    type State = (Dep1#State, Dep2#State)
  }

  /*
   type arguments [oss.giussi.cappio.impl.bcast.UniformReliableBroadcast.DependencyMod[P],oss.giussi.cappio.impl.bcast.UniformReliableBroadcast.URBData[P]]
   do not conform to trait ModS's type parameter bounds [M <: oss.giussi.cappio.Mod{type Payload = P},P]
   */
  trait URBMod[P] extends ModS[URBDep[P]] { // aca hay algo q huele mal
    override type Ind = URBDeliver[P]
    override type Req = URBBcast[P]
    override type S = URBState[P]
  }

  case class URBData[T](sender: ProcessId, msg: T)

  // TODO
  val BEB = Instance("beb")
  val PFD = Instance("pfd")

  object URBState {
    def init[P](self: ProcessId, all: Set[ProcessId], timeout: Int) = {
      val pfdMod = PerfectFailureDetector.init(self, all, timeout) // Este timeout deberia ser enviado en un metodo Init al comienzo asi todos tienen el mismo!
      val bebMod = BestEffortBroadcast.init[URBData[P]](self, all, timeout)
      val combined: Module[URBDep[P]] = CombinedModule.paired(PFD, pfdMod, BEB, bebMod)
      URBState(Set.empty, Set.empty[(ProcessId, UUID, P)], all, Map.empty, combined)
    }
  }

  case class URBState[P](delivered: Set[UUID], pending: Set[(ProcessId, UUID, P)], correct: Set[ProcessId], acks: Acks, module: Module[URBDep[P]]) extends StateWithModule[URBDep[P], URBState[P]] {
    override def updateModule(m: Module[URBDep[P]]): URBState[P] = copy(module = m)

    private def canDeliver(m: UUID): Boolean = correct -- acks(m) isEmpty

    def addPending(from: ProcessId, id: UUID, msg: P) = copy(pending = pending + Tuple3(from, id, msg))

    def crashed(p: ProcessId) = copy(correct = correct - p)

    def ack(from: ProcessId, sender: ProcessId, id: UUID, msg: P): (URBState[P], Option[(ProcessId, UUID, P)]) = {
      val nacks = acks.updated(id, acks.get(id).map(_ + from).getOrElse(Set(from)))
      val (np, triggers) = {
        val tuple = (sender, id, msg)
        if (!pending.contains(tuple)) (pending + tuple, Some(tuple))
        else (pending, None)
      }
      (copy(acks = nacks, pending = np), triggers)
    }

    // TODO N indications o 1 indication?
    // tendria que inventar un evento que para evaluar conditions si quiero disparar todos, porque el trigger afecta el estado.
    // o agregar logica para que elimine duplicados por payload.id
    def evaluateCondition(): Option[(URBState[P], (ProcessId, P))] = {
      val candidates = pending.filter(p => canDeliver(p._2) && !delivered.contains(p._2)).toList // random element
      if (candidates.isEmpty) None
      else {
        val (sender, id, msg) = candidates(Random.nextInt(candidates.length))
        Some((copy(delivered = delivered + id), (sender, msg)))
      }
    }
  }

  case class URBBcast[P](payload: Payload[P])

  case class URBDeliver[P](from: ProcessId, payload: P)

  def init[P](self: ProcessId, all: Set[ProcessId], timeout: Int) = UniformReliableBroadcast(self, URBState.init[P](self, all, timeout))

  def processLocal[P](self: ProcessId)(implicit inj1: Inject[URBDep[P]#Req, URBDep[P]#Dep1#Req], inj2: Inject[URBDep[P]#Req, URBDep[P]#Dep2#Req]): ProcessLocal[URBBcast[P], URBState[P], URBDeliver[P], URBDep[P]#Req, URBDep[P]#Ind,URBDep[P]#Payload] = new processLocalHelper2[URBMod[P],URBDep[P]]{
    override def onPublicRequest(req: URBBcast[P], state: State): Output = {
      val URBBcast(p) = req
      val reqs = Set(req2(BebBcast(Payload(p.id, URBData(self, p.msg)), BEB)))
      LocalStep.withRequests(reqs, state.addPending(self, p.id, p.msg))
    }

    override def onDependencyIndication1(ind: Crashed, state: State): Output = LocalStep.withState(state.crashed(ind.id))

    override def onDependencyIndication2(ind: BebDeliver[URBData[P]], state: State): Output = {
      val BebDeliver(from, Payload(id, URBData(sender, msg))) = ind
      val (ns, triggers) = state.ack(from, sender, id, msg)
      val req = triggers.map { case (pid, uuid, msg) => req2(BebBcast(Payload(uuid, URBData(pid, msg)), BEB)) }.toSet // aca el Self se lo va a estar poniendo la abstracion PerfectLink
      LocalStep.withRequests(req, ns)
    }

    override def onTick(state: State): Output = {
      state.evaluateCondition() match {
        case Some((ns, (sender, msg))) => LocalStep.withIndications(Set(URBDeliver(sender, msg)), ns)
        case None => LocalStep.withState(state)
      }
    }
  }
}

case class UniformReliableBroadcast[T](self: ProcessId, state: URBState[T]) extends AbstractModule[URBMod[T], URBDep[T]] {

  override def copyModule(s: URBState[T]) = copy(state = s)

  override val processLocal: PLocal = UniformReliableBroadcast.processLocal(self)
}
