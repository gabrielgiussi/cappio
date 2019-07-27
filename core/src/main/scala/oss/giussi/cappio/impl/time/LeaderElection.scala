package oss.giussi.cappio.impl.time

import oss.giussi.cappio.Messages.ProcessLocal
import oss.giussi.cappio._
import oss.giussi.cappio.impl.time.LeaderElection.{LEMod, LEState, Leader}
import oss.giussi.cappio.impl.time.PerfectFailureDetector.{Crashed, PFDMod, PFDState}

object LeaderElection {
  case class Leader(p: ProcessId)

  object LEState {
    def init(all: Set[ProcessId], module: Module[PFDMod]) = new LEState(LEState.maxrank(all),Set.empty,all,module)

    def maxrank(alive: Set[ProcessId]) = alive.minBy(_.id)
  }

  type LEMod = ModS[PFDMod]{
  type S = LEState
  type Req = NoRequest
  type Ind = Leader
}

  /*
   TODO leader on init or on first tick (I could add the evaluation on each tick)?
   el problema de esto es que nunca voy a tener la indication Leader(0)
   tal vez deberia esperar al primer tick o agregar un evento Init donde se dispare esto.
   */
  case class LEState(leader: ProcessId, suspected: Set[ProcessId], all: Set[ProcessId], module: Module[PFDMod]) extends StateWithModule[PFDMod,LEState]{

    def suspect(p: ProcessId): (LEState,Option[ProcessId]) = {
      val s = suspected + p
      val nl = LEState.maxrank(all.diff(s))
      (copy(suspected = s),Some(nl).filterNot(_ == leader))
    }

    override def updateModule(m: Module[PFDMod]): LEState = copy(module = m)

  }

  def init(self: ProcessId,all: Set[ProcessId],timeout: Int) = LeaderElection(LEState.init(all,PerfectFailureDetector.init(self,all,timeout)))

  def processLocal: ProcessLocal[NoRequest,LEState,Leader,NoRequest,Crashed] = {
    import oss.giussi.cappio.Messages._
    (msg,state) => msg match {
      case PublicRequest(_) => LocalStep.withState(state) // TODO
      case LocalIndication(Crashed(p)) =>
        val (ns,nl) = state.suspect(p)
        /*
        TODO no tiene sentido en el mismo turno devolver dos Indications de Leader, podria poner un "hack" para quedarme con el de rango menor (que se supone que fue el ultimo)
        pero lo tengo que hacer en una especie de hook beforeReturnNextState porque a esta altura no tengo todas las indications
        */
        LocalStep.withIndications(nl.map(Leader).toSet,ns)
      case Tick => LocalStep.withState(state) //  TODO make process local a partial function ??
    }
  }
}

case class LeaderElection(state: LEState) extends AbstractModule[LEMod,PFDMod] {
  override def copyModule(s: LEState) = copy(state = s)

  override val processLocal: PLocal = LeaderElection.processLocal
}
