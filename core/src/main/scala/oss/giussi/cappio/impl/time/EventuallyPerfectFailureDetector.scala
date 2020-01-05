package oss.giussi.cappio.impl.time

import oss.giussi.cappio.Messages.ProcessLocal
import oss.giussi.cappio._
import oss.giussi.cappio.impl.net.PerfectLink
import oss.giussi.cappio.impl.net.PerfectLink.{PLDeliver, PLModule, PLSend}
import oss.giussi.cappio.impl.time.EventuallyPerfectFailureDetector.EPFDState.EPFDS

object EventuallyPerfectFailureDetector {

  sealed trait EPFDIndication

  case class Suspect(id: ProcessId) extends EPFDIndication

  case class Restore(id: ProcessId) extends EPFDIndication

  type EPFDep = PLModule[HeartbeatMsg]

  type EPFDMod = ModS[EPFDep] {
    type Req = NoRequest
    type Ind = EPFDIndication
    type S = EPFDStateInternal
  }

  object EPFDState {
    type EPFDS = StateWithModule[EPFDep,EPFDStateInternal]

    def init(all: Set[ProcessId], delay: Int): EPFDS = StateWithModule(PerfectLink.init(delay),EPFDStateInternal(0, all, Set.empty, all, delay, delay)) // TODO el delay es el mismo?
  }

  case class EPFDStateInternal(timer: Int, alive: Set[ProcessId], suspected: Set[ProcessId], all: Set[ProcessId], currentDelay: Int, delay: Int) {
    def tick(): EPFDStateInternal = copy(timer = timer + 1)

    def hearbeat(id: ProcessId): EPFDStateInternal = copy(alive = alive + id)

    def timeout() = {
      val nd = if (!alive.intersect(suspected).isEmpty) currentDelay + delay else currentDelay
      val newSuspected = all.filter(p => !alive.contains(p) && !suspected.contains(p))
      val revived = all.filter(p => alive.contains(p) && suspected.contains(p))
      (copy(alive = Set.empty, timer = 0, currentDelay = nd, suspected = suspected ++ newSuspected -- revived), newSuspected, revived)
    }
  }

  def processLocal(self: ProcessId, all: Set[ProcessId], instance: Instance): ProcessLocal[NoRequest, EPFDS, EPFDIndication, EPFDep#Req, EPFDep#Ind,EPFDep#Payload] = {
    import oss.giussi.cappio.Messages._
    (msg, state) =>
      msg match {
        case Tick if (state.state.timer + 1 == state.state.currentDelay) =>
          val (ns, suspected, revived) = state.state.timeout()
          val ind: Set[EPFDIndication] = suspected.map(Suspect) ++ revived.map(Restore)
          val heartbeats = all.map(to => LocalRequest(PLSend(Packet[HeartbeatMsg](self, to, HeartbeatRequest, instance))))
          LocalStep.withRequestsAndIndications(ind, heartbeats, state.updateState(ns))
        case Tick => LocalStep.withState(state.updateState(_.tick))
        case LocalIndication(PLDeliver(Packet(_, HeartbeatReply, from, _, _))) => LocalStep.withState(state.updateState(_.hearbeat(from)))
        case LocalIndication(PLDeliver(Packet(_, HeartbeatRequest, from, _, _))) => LocalStep.withRequests(Set(LocalRequest(PLSend(Packet(self, from, HeartbeatReply, instance)))), state)
      }
  }

  def apply[T](all: Set[ProcessId], timeout: Int)(self: ProcessId): Module[EPFDMod] = {
    AbstractModule.mod[EPFDMod,EPFDMod#Dep](EPFDState.init(all - self,timeout),processLocal(self,all - self,null))
  }
}