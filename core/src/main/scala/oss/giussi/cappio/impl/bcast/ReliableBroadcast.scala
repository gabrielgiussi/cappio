package oss.giussi.cappio.impl.bcast

import oss.giussi.cappio._
import oss.giussi.cappio.impl.AppState
import oss.giussi.cappio.impl.AppState.AppMod2
import oss.giussi.cappio.impl.bcast.BestEffortBroadcast.{BebBcast, BebDeliver, BebMod}
import oss.giussi.cappio.impl.time.{Crashed, PerfectFailureDetector}
import oss.giussi.cappio.impl.time.PerfectFailureDetector.PFDMod
import shapeless.ops.coproduct.Inject

object ReliableBroadcast {
  val BEB = Instance("beb")
  val PFD = Instance("pfd")

  type RBDep[P] = Mod2 {
    type Dep1 = PFDMod
    type Dep2 = BebMod[RBData[P]]
    type State = (Dep1#State, Dep2#State)
  }

  type RBMod[P] = ModS[RBDep[P]] {
    type S = RBcastState[P]
    type Ind = RBDeliver[P]
    type Req = RBBcast[P]
  }

  object RBBcast {
    def apply[P](msg: P): RBBcast[P] = new RBBcast(Payload(msg))
  }

  case class RBBcast[P](payload: Payload[P])

  case class RBDeliver[P](from: ProcessId, payload: Payload[P])

  case class RBData[P](sender: ProcessId, msg: P)

  object RBcastState {
    def init[P](self: ProcessId, all: Set[ProcessId], timeout: Int): StateWithModule[RBMod[P]#Dep,RBMod[P]#S] = {
      val pfdm = PerfectFailureDetector(all, timeout)(self)
      val bebm = BestEffortBroadcast[RBData[P]](all, timeout)(self)
      val state = RBcastState(all.map(_ -> Set.empty[Payload[P]]).toMap, all)
      val dep = CombinedModule.paired(PFD, pfdm, BEB, bebm)
      StateWithModule(dep,state)
    }
  }

  case class RBcastState[P](delivered: Map[ProcessId, Set[Payload[P]]], correct: Set[ProcessId]) {

    def crashed(id: ProcessId): (RBcastState[P], Set[Payload[P]]) = (copy(correct = correct - id), delivered(id))

    def deliver(sender: ProcessId, payload: Payload[P]) = {
      if (delivered(sender).contains(payload)) None
      else {
        val ns = copy(delivered = delivered.updated(sender, delivered(sender) + payload))
        val toBcast = if (correct.contains(sender)) Set.empty else Set(payload)
        Some((ns, toBcast))
      }
    }
  }

  import oss.giussi.cappio.Messages._

  def processLocal[P](self: ProcessId)(implicit inj1: Inject[RBDep[P]#Req, RBDep[P]#Dep1#Req], inj2: Inject[RBDep[P]#Req, RBDep[P]#Dep2#Req]) = new ProcessLocalHelper2[RBMod[P],RBDep[P]]{

    override def onPublicRequest(req: RBBcast[P], state: State): Output = LocalStep.withRequests(Set(req2(BebBcast(Payload(req.payload.id, RBData(self, req.payload.msg)), ReliableBroadcast.BEB))), state)

    override def onDependencyIndication1(ind: Crashed, state: State): Output = {
      val (ns, toBcast) = state.state.crashed(ind.id)
      val requests = toBcast.map { case Payload(uuid, msg) => req2(BebBcast(Payload(uuid, RBData(ind.id, msg)), ReliableBroadcast.BEB)) }
      LocalStep.withRequests(requests, state.updateState(ns))
    }

    override def onDependencyIndication2(ind: BebDeliver[RBData[P]], state: State): Output = {
      val BebDeliver(_, Payload(id, RBData(sender, msg))) = ind
      state.state.deliver(sender, Payload(id,msg)) match {
        case Some((ns, bcast)) =>
          val ind = Set(RBDeliver(sender, Payload(id, msg)))
          val req = bcast.map { case Payload(uuid, p) => req2(BebBcast(Payload(uuid, RBData(self,p)), ReliableBroadcast.BEB)) }
          LocalStep.withRequestsAndIndications(ind, req, state.updateState(ns))
        case None => LocalStep.withState(state)
      }
    }
  }

  def apply[T](all: Set[ProcessId], timeout: Int)(self: ProcessId): Module[RBMod[T]] = {
    AbstractModule.mod[RBMod[T],RBMod[T]#Dep](RBcastState.init[T](self,all,timeout),ReliableBroadcast.processLocal[T](self))
  }

  type RBApp[P] = AppMod2[P,RBMod[P]]

  def app[P](all: Set[ProcessId], timeout: Int)(self: ProcessId): Module[RBApp[P]] = {
    val rb = ReliableBroadcast[P](all,timeout)(self)
    AppState.app2[P,RBMod[P]](rb,(state,ind) => Some(ind.payload.msg))
  }
}