package oss.giussi.cappio.impl.time

import oss.giussi.cappio.Messages.ProcessLocal
import oss.giussi.cappio._
import oss.giussi.cappio.impl.net.PerfectLink
import oss.giussi.cappio.impl.net.PerfectLink.{PLDeliver, PLModule, PLSend}

object PerfectFailureDetector {

  type PFDMod = ModS[PLModule[HeartbeatMsg]] {
    type Req = NoRequest
    type Ind = Crashed
    type S = PFDState
  }

  object PFDState {
    def init(all: Set[ProcessId], timeout: Int): StateWithModule[PLModule[HeartbeatMsg], PFDState] = StateWithModule(PerfectLink.init(timeout),PFDState(0,all,Set.empty,all))
  }

  case class PFDState(timer: Int, alive: Set[ProcessId], detected: Set[ProcessId], all: Set[ProcessId]) {
    protected[time] def tick() = copy(timer = timer + 1)

    protected[time] def timeout() = {
      val newCrashed = all.filter(id => !alive.contains(id) && !detected.contains(id))
      val allCrashed = detected ++ newCrashed
      val survivors = all -- allCrashed
      (copy(alive = Set.empty, detected = allCrashed, timer = 0), newCrashed, survivors)
    }

    protected[time] def hearbeat(id: ProcessId): PFDState = copy(alive = alive + id)

  }

  def processLocal(self: ProcessId, timeout: Int, instance: Instance): ProcessLocal[NoRequest,PFDMod#State,Crashed,PLSend[HeartbeatMsg],PLDeliver[HeartbeatMsg],HeartbeatMsg] = {
    import oss.giussi.cappio.Messages._
    (msg,state) => msg match {
      case PublicRequest(_) => LocalStep.withState(state)
      case Tick if state.state.timer + 1 == timeout =>
        val (ns, crashed, alive) = state.state.timeout()
        val ind = crashed.map(Crashed)
        val heartbeats = alive.map(id => LocalRequest(PLSend[HeartbeatMsg](Packet(self, id, HeartbeatRequest, instance))))
        LocalStep.withRequestsAndIndications(ind, heartbeats, state.updateState(ns))
      case Tick => LocalStep.withState(state.updateState(_.tick))
      case LocalIndication(PLDeliver(Packet(_, HeartbeatReply, from, _, _))) => LocalStep.withState(state.updateState(_.hearbeat(from)))
      case LocalIndication(PLDeliver(Packet(_, HeartbeatRequest, from, _, _))) => LocalStep.withRequests(Set(LocalRequest(PLSend(Packet(self, from, HeartbeatReply, instance)))), state)
    }
  }

  def apply[T](all: Set[ProcessId], timeout: Int)(self: ProcessId): Module[PFDMod] = {
    AbstractModule.mod[PFDMod,PFDMod#Dep](PFDState.init(all - self,timeout),processLocal(self,timeout,null))
  }
}