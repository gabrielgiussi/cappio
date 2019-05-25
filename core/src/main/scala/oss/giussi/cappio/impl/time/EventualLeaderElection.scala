package oss.giussi.cappio.impl.time

import oss.giussi.cappio.impl.net.Socket
import oss.giussi.cappio.impl.time.EventualLeaderElection.{ELEState, Trust}
import oss.giussi.cappio.impl.time.EventuallyPerfectFailureDetector.{EPFDIndication, EPFDState, Restore, Suspect}
import oss.giussi.cappio._

object EventualLeaderElection {
  case class Trust(id: ProcessId)

  case class ELEState(leader: ProcessId, suspected: Set[ProcessId], all: Set[ProcessId], module: Module[NoRequest,EPFDState,EPFDIndication]) extends StateWithModule[NoRequest,EPFDState,EPFDIndication,ELEState] {
    override def updateModule(m: Module[NoRequest, EPFDState, EPFDIndication]): ELEState = copy(module = m)

    private def findLeader(s: Set[ProcessId]) = {
      val nl = maxrank(all.diff(s))
      (copy(suspected = s),Some(nl).filterNot(_ == leader))
    }

    def restore(p: ProcessId): (ELEState,Option[ProcessId]) = findLeader(suspected - p)

    def suspect(p: ProcessId): (ELEState,Option[ProcessId]) = findLeader(suspected + p)
  }

}

case class EventualLeaderElection(self: ProcessId, state: ELEState) extends AbstractModule[NoRequest,ELEState,Trust,NoRequest,EPFDState,EPFDIndication] {
  override def copyModule(s: ELEState) = copy(state = s)

  override def processLocal(l: LocalMsg, state: ELEState): LocalStep = l match {
    case PublicRequest(_) => LocalStep(state)
    case Tick => LocalStep(state) // TODO make LocalStep require another type (wrapper of state to avoid using the state of the Module)
    case LocalIndication(ind) =>
      val (ns,nl) = ind match {
        case Suspect(id) => state.suspect(id)
        case Restore(id) => state.restore(id)
      }
      LocalStep(nl.map(Trust).toSet,ns)

  }

}
