package oss.giussi.cappio.impl.time

import oss.giussi.cappio.Messages.ProcessLocal
import oss.giussi.cappio.impl.net.PerfectLink
import oss.giussi.cappio.impl.net.PerfectLink.{PLDeliver, PLModule, PLSend}
import oss.giussi.cappio.impl.time.PerfectFailureDetector.{PFDMod, PFDState}
import oss.giussi.cappio._

object PerfectFailureDetector {

  sealed trait HeartbeatMsg

  case object HeartbeatRequest extends HeartbeatMsg

  case object HeartbeatReply extends HeartbeatMsg

  case class Crashed(id: ProcessId)

  type PFDMod = ModS[PLModule[HeartbeatMsg]] {
    type Req = NoRequest
    type Ind = Crashed
    type S = PFDState
  }

  object PFDState {
    def init(all: Set[ProcessId], timeout: Int) = PFDState(0,all,Set.empty,all,PerfectLink.init(timeout))
  }

  case class PFDState(timer: Int, alive: Set[ProcessId], detected: Set[ProcessId], all: Set[ProcessId], module: Module[PFDMod#Dep]) extends StateWithModule[PFDMod#Dep,PFDState] {
    override def updateModule(m: Module[PFDMod#Dep]): PFDState = copy(module = m)

    protected[time] def tick() = copy(timer = timer + 1)

    protected[time] def timeout() = {
      val newCrashed = all.filter(id => !alive.contains(id) && !detected.contains(id))
      val allCrashed = detected ++ newCrashed
      val survivors = all -- allCrashed
      (copy(alive = Set.empty, detected = allCrashed, timer = 0), newCrashed, survivors)
    }

    protected[time] def hearbeat(id: ProcessId): PFDState = copy(alive = alive + id)

  }

  def init(self: ProcessId, all: Set[ProcessId], timeout: Int) = PerfectFailureDetector(self,PFDState.init(all - self,timeout),timeout,new Instance("TODO")) // TODO

  def processLocal(self: ProcessId, timeout: Int, instance: Instance): ProcessLocal[NoRequest,PFDState,Crashed,PLSend[HeartbeatMsg],PLDeliver[HeartbeatMsg],HeartbeatMsg] = {
    import oss.giussi.cappio.Messages._
    (msg,state) => msg match {
      case PublicRequest(_) => LocalStep.withState(state)
      case Tick if state.timer + 1 == timeout =>
        val (ns, crashed, alive) = state.timeout()
        val ind = crashed.map(Crashed)
        val heartbeats = alive.map(id => LocalRequest(PLSend[HeartbeatMsg](Packet(self, id, HeartbeatRequest, instance))))
        LocalStep.withRequestsAndIndications(ind, heartbeats, ns)
      case Tick => LocalStep.withState(state.tick())
      case LocalIndication(PLDeliver(Packet(_, HeartbeatReply, from, _, _))) => LocalStep.withState(state.hearbeat(from))
      case LocalIndication(PLDeliver(Packet(_, HeartbeatRequest, from, _, _))) => LocalStep.withRequests(Set(LocalRequest(PLSend(Packet(self, from, HeartbeatReply, instance)))), state)
    }
  }
}

case class PerfectFailureDetector(self: ProcessId, state: PFDState, timeout: Int, instance: Instance) extends AbstractModule[PFDMod,PFDMod#Dep] {
  override def copyModule(ns: PFDState) = copy(state = ns)

  override val processLocal: PLocal = PerfectFailureDetector.processLocal(self,timeout,instance)
}