package oss.giussi.cappio.impl.bcast

import java.util.UUID

import oss.giussi.cappio.Messages.{LocalStep, ProcessLocal}
import oss.giussi.cappio._
import oss.giussi.cappio.impl.bcast.BestEffortBroadcast.{BebBcast, BebDeliver, BebMod}
import oss.giussi.cappio.impl.time.{Crashed, PerfectFailureDetector}
import oss.giussi.cappio.impl.time.PerfectFailureDetector.PFDMod
import shapeless.ops.coproduct.Inject

import scala.util.Random


object UniformReliableBroadcast {

  type Acks = Map[UUID, Set[ProcessId]]


  type URBDep[P] = Mod2 {
    type Dep1 = PFDMod
    type Dep2 = BebMod[URBData[P]]
    type State = (Dep1#State, Dep2#State)
  }

  type URBMod[P] = ModS[URBDep[P]] {
    type Ind = URBDeliver[P]
    type Req = URBBcast[P]
    type S = URBState[P]
  }

  case class URBData[T](sender: ProcessId, msg: T)

  // TODO
  val BEB = Instance("beb")
  val PFD = Instance("pfd")

  object URBState {
    def init[P](self: ProcessId, all: Set[ProcessId], timeout: Int): StateWithModule[URBDep[P], URBState[P]] = {
      val pfdMod = PerfectFailureDetector(all, timeout)(self) // Este timeout deberia ser enviado en un metodo Init al comienzo asi todos tienen el mismo!
      val bebMod = BestEffortBroadcast[URBData[P]](all, timeout)(self)
      val combined: Module[URBDep[P]] = CombinedModule.paired(PFD, pfdMod, BEB, bebMod)
      StateWithModule(combined,URBState(Set.empty, Set.empty[(ProcessId, UUID, P)], all, Map.empty))
    }
  }

  case class URBState[P](delivered: Set[UUID], pending: Set[(ProcessId, UUID, P)], correct: Set[ProcessId], acks: Acks) {

    private def canDeliver(m: UUID): Boolean = acks.get(m).map(correct -- _ isEmpty).getOrElse(false)

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
    def evaluateCondition: Option[(URBState[P], (ProcessId, P))] = {
      val candidates = pending.filter(p => canDeliver(p._2) && !delivered.contains(p._2)).toList // random element
      if (candidates.isEmpty) None
      else {
        val (sender, id, msg) = candidates(Random.nextInt(candidates.length))
        Some((copy(delivered = delivered + id), (sender, msg)))
      }
    }
  }

  object URBBcast {
    def apply[P](msg: P): URBBcast[P] = new URBBcast(Payload(msg))
  }

  case class URBBcast[P](payload: Payload[P]) {
    override def toString: String = s"urb-bcast ${payload.msg}"
  }

  case class URBDeliver[P](from: ProcessId, payload: P)

  def processLocal[P](self: ProcessId)(implicit inj1: Inject[URBDep[P]#Req, URBDep[P]#Dep1#Req], inj2: Inject[URBDep[P]#Req, URBDep[P]#Dep2#Req]): ProcessLocal[URBBcast[P], URBMod[P]#State, URBDeliver[P], URBDep[P]#Req, URBDep[P]#Ind,URBDep[P]#Payload] = new ProcessLocalHelper2[URBMod[P],URBDep[P]]{
    override def onPublicRequest(req: URBBcast[P], state: State): Output = {
      val URBBcast(p) = req
      val reqs = Set(req2(BebBcast(Payload(p.id, URBData(self, p.msg)), BEB)))
      LocalStep.withRequests(reqs, state.updateState(_.addPending(self, p.id, p.msg)))
    }

    override def onDependencyIndication1(ind: Crashed, state: State): Output = LocalStep.withState(state.updateState(_.crashed(ind.id)))

    override def onDependencyIndication2(ind: BebDeliver[URBData[P]], state: State): Output = {
      val BebDeliver(from, Payload(id, URBData(sender, msg))) = ind
      val (ns, triggers) = state.state.ack(from, sender, id, msg)
      val req = triggers.map { case (pid, uuid, msg) => req2(BebBcast(Payload(uuid, URBData(pid, msg)), BEB)) }.toSet // aca el Self se lo va a estar poniendo la abstracion PerfectLink
      LocalStep.withRequests(req, state.updateState(ns))
    }

    override def onTick(state: State): Output = {
      state.state.evaluateCondition match {
        case Some((ns, (sender, msg))) => LocalStep.withIndications(Set(URBDeliver(sender, msg)), state.updateState(ns))
        case None => LocalStep.withState(state)
      }
    }
  }

  def apply[P](all: Set[ProcessId], timeout: Int)(self: ProcessId): Module[URBMod[P]] = AbstractModule.mod[URBMod[P],URBMod[P]#Dep](URBState.init[P](self,all,timeout),processLocal(self))
}
