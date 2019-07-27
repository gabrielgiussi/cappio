package oss.giussi.cappio.impl.time

import oss.giussi.cappio._
import oss.giussi.cappio.impl.time.EventualLeaderElection.{ELEMod, ELEState, Trust}
import oss.giussi.cappio.impl.time.EventuallyPerfectFailureDetector.{EPFDIndication, EPFDMod, EPFDState, Restore, Suspect}

object EventualLeaderElection {
  case class Trust(id: ProcessId)

  case class ELEState(leader: ProcessId, suspected: Set[ProcessId], all: Set[ProcessId], module: Module[EPFDMod]) extends StateWithModule[EPFDMod, ELEState] {
    override def updateModule(m: Module[EPFDMod]): ELEState = copy(module = m)

    private def findLeader(s: Set[ProcessId]) = {
      val nl = maxrank(all.diff(s))
      (copy(suspected = s),Some(nl).filterNot(_ == leader))
    }

    def restore(p: ProcessId): (ELEState,Option[ProcessId]) = findLeader(suspected - p)

    def suspect(p: ProcessId): (ELEState,Option[ProcessId]) = findLeader(suspected + p)
  }

  type ELEMod = ModS[EPFDMod] {
  type S = ELEState
  type Req = NoRequest
  type Ind = Trust
}

}

case class EventualLeaderElection(self: ProcessId, state: ELEState) extends AbstractModule[ELEMod,EPFDMod] {
  import Messages._
  override def copyModule(s: ELEState) = copy(state = s)

  def processLocal(l: LMsg, state: ELEState): LStep = l match {
    case PublicRequest(_) => LocalStep.withState(state)
    case Tick => LocalStep.withState(state) // TODO make LocalStep require another type (wrapper of state to avoid using the state of the Module)
    case LocalIndication(ind) =>
      val (ns,nl) = ind match {
        case Suspect(id) => state.suspect(id)
        case Restore(id) => state.restore(id)
      }
      LocalStep.withIndications(nl.map(Trust).toSet,ns)

  }

  override val processLocal: PLocal = ???
}
