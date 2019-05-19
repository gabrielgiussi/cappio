package oss.giussi.cappio.impl.time

import oss.giussi.cappio.impl.net.Socket
import oss.giussi.cappio.impl.time.LeaderElection.{LEState, Leader}
import oss.giussi.cappio.impl.time.PerfectFailureDetector.{Crashed, PFDState}
import oss.giussi.cappio.{AbstractModule, Module, NoRequest, ProcessId, StateWithModule}

object LeaderElection {
  case class Leader(p: ProcessId)

  object LEState {
    def init(all: Set[ProcessId], module: Module[NoRequest,PFDState,Crashed]) = new LEState(LEState.maxrank(all),Set.empty,all,module)

    def maxrank(alive: Set[ProcessId]) = alive.minBy(_.id)
  }

  /*
   TODO leader on init or on first tick (I could add the evaluation on each tick)?
   el problema de esto es que nunca voy a tener la indication Leader(0)
   tal vez deberia esperar al primer tick o agregar un evento Init donde se dispare esto.
   */
  case class LEState(leader: ProcessId, suspected: Set[ProcessId], all: Set[ProcessId], module: Module[NoRequest,PFDState,Crashed]) extends StateWithModule[NoRequest,PFDState,Crashed,LEState]{

    def suspect(p: ProcessId): (LEState,Option[ProcessId]) = {
      val s = suspected + p
      val nl = LEState.maxrank(all.diff(s))
      (copy(suspected = s),Some(nl).filterNot(_ == leader))
    }

    override def updateModule(m: Module[NoRequest, PFDState, Crashed]): LEState = copy(module = m)

  }

  def init(self: ProcessId,all: Set[ProcessId],timeout: Int) = LeaderElection(LEState.init(all,PerfectFailureDetector.init(self,all,timeout)))
}

case class LeaderElection(state: LEState) extends AbstractModule[NoRequest,LEState,Leader,NoRequest,PFDState,Crashed] {
  override def copyModule(s: LEState): AbstractModule[NoRequest, LEState, Leader, NoRequest, PFDState, Crashed] = copy(state = s)

  override def processLocal(l: LocalMsg, state: LEState): LocalStep = l match {
    case PublicRequest(_) => LocalStep(state) // TODO
    case LocalIndication(Crashed(p)) =>
      val (ns,nl) = state.suspect(p)
      /*
      TODO no tiene sentido en el mismo turno devolver dos Indications de Leader, podria poner un "hack" para quedarme con el de rango menor (que se supone que fue el ultimo)
      pero lo tengo que hacer en una especie de hook beforeReturnNextState porque a esta altura no tengo todas las indications
      */
      LocalStep(nl.map(Leader).toSet,ns)
    case Tick => LocalStep(state) //  TODO make process local a partial function ??
  }

  override def t: Socket[NoRequest, PFDState, Crashed] = state.module.tail

}
