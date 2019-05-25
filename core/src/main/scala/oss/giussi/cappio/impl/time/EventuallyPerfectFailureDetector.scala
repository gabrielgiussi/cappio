package oss.giussi.cappio.impl.time

import oss.giussi.cappio.impl.net.PerfectLink.{PLDeliver, PLSend}
import oss.giussi.cappio.impl.net.PerfectLinkBeta.PerfectLinkBetaState
import oss.giussi.cappio.impl.net.{PerfectLinkBeta, Socket}
import oss.giussi.cappio.impl.time.EventuallyPerfectFailureDetector.{EPFDIndication, EPFDState, Restore, Suspect}
import oss.giussi.cappio.impl.time.PerfectFailureDetector.{HeartbeatReply, HeartbeatRequest}
import oss.giussi.cappio.{AbstractModule, Instance, Module, NoRequest, Packet, ProcessId, StateWithModule}

object EventuallyPerfectFailureDetector {
  sealed trait EPFDIndication
  case class Suspect(id: ProcessId) extends EPFDIndication

  case class Restore(id: ProcessId) extends EPFDIndication

  object EPFDState {
    def init(all: Set[ProcessId],delay: Int): EPFDState = EPFDState(0,all,Set.empty,all,delay,delay,PerfectLinkBeta.init(delay)) // TODO el delay es el mismo?
  }

  case class EPFDState(timer: Int, alive: Set[ProcessId], suspected: Set[ProcessId], all: Set[ProcessId], currentDelay: Int, delay: Int, module: Module[PLSend,PerfectLinkBetaState,PLDeliver]) extends StateWithModule[PLSend,PerfectLinkBetaState,PLDeliver,EPFDState] {
    override def updateModule(m: Module[PLSend, PerfectLinkBetaState, PLDeliver]): EPFDState = copy(module = m)

    def tick(): EPFDState = copy(timer = timer + 1)

    def hearbeat(id: ProcessId): EPFDState = copy(alive = alive + id)

    def timeout() = {
      val nd = if (!alive.intersect(suspected).isEmpty) currentDelay + delay else currentDelay
      val newSuspected = all.filter(p => !alive.contains(p) && !suspected.contains(p))
      val revived = all.filter(p => alive.contains(p) && suspected.contains(p))
      (copy(alive = Set.empty, timer = 0, currentDelay = nd, suspected = suspected ++ newSuspected -- revived), newSuspected, revived)
    }
  }

  def init(self: ProcessId, all: Set[ProcessId], delay: Int): EventuallyPerfectFailureDetector = EventuallyPerfectFailureDetector(self,all - self,EPFDState.init(all - self,delay),new Instance("")) // TODO
}

case class EventuallyPerfectFailureDetector(self: ProcessId, all: Set[ProcessId], state: EPFDState, instance: Instance) extends AbstractModule[NoRequest,EPFDState,EPFDIndication,PLSend,PerfectLinkBetaState,PLDeliver] {
  override def copyModule(ns: EPFDState): AbstractModule[NoRequest, EPFDState, EPFDIndication, PLSend, PerfectLinkBetaState, PLDeliver] = copy(state = ns)

  override def processLocal(l: LocalMsg, state: EPFDState): LocalStep = l match {
    case Tick if (state.timer + 1 == state.currentDelay) =>
      val (ns,suspected,revived) = state.timeout()
      val ind: Set[EPFDIndication] = suspected.map(Suspect) ++ revived.map(Restore)
      val heartbeats = all.map(to => LocalRequest(PLSend(Packet(self,to,HeartbeatRequest,instance))))
      LocalStep.prueba(ind,heartbeats,ns)
    case Tick => LocalStep(state.tick)
    case LocalIndication(PLDeliver(Packet(_,HeartbeatReply,from,_,_))) => LocalStep(state.hearbeat(from))
    case LocalIndication(PLDeliver(Packet(_,HeartbeatRequest,from,_,_))) => LocalStep.localRequest(Set(LocalRequest(PLSend(Packet(self,from,HeartbeatReply,instance)))),state)

  }

}
