package oss.ggiussi.cappio.impl.faildet

import oss.ggiussi.cappio.core.LinkProtocol._
import oss.ggiussi.cappio.{InstanceID, ProcessID, Processes}
import oss.ggiussi.cappio.core._
import oss.ggiussi.cappio.impl.{Instances, Triggers}
import oss.ggiussi.cappio.impl.faildet.PerfectFailureDetectorProtocol._


object PerfectFailureDetectorProtocol {

  // TODO this is spcecial action because the user shoulnd't send this action. I could associate this to the synchronous model and then send it if this model is chosen.
  // However, I could use the tick in all automaton also for calculate the step!
  case object Tick extends ActionHeader with NoPayloadAction {
    override val instance: InstanceID = InstanceID("tick")

    override def header: ActionHeader = this

  }

  case class Timeout(instance: InstanceID) extends SimpleAction

  case class Crashed(p: ProcessID, instance: InstanceID) extends SimpleAction

  sealed trait Heartbeat

  case class HeartbeatRequest(q: ProcessID) extends Heartbeat

  case class HeartbeatReply(p: ProcessID) extends Heartbeat

}

object PFDState {
  def init(id: ProcessID, timer: Int, instance: InstanceID)(implicit processes: Set[ProcessID]): PFDState = new PFDState(processes - id, Set.empty, Triggers.init(), id, instance, 0, timer)
}

case class PFDState(alive: Set[ProcessID], detected: Set[ProcessID], triggers: Triggers, id: ProcessID, instance: InstanceID, clock: Int, timer: Int)(implicit processes: Set[ProcessID]) {
  val neighbors = processes - id

  def shouldTimeout(): Boolean = (clock % timer == 0)

  def tick(): NextState[PFDState] = {
    val next = copy(clock = clock + 1)
    NextState(next, if (next.shouldTimeout()) Set(Timeout(instance)) else Set.empty)
  }

  def timeout(): NextState[PFDState] = {
    val crashedProcesses = neighbors.filter(p => !(alive contains p) && !(detected contains p))
    val requests: Set[Action] = neighbors.map(n => LinkProtocol.send(id, n, Instances.FAILURE_DET_LINK, Message(HeartbeatRequest(id))))
    val crashed: Set[Action] = crashedProcesses.map(Crashed(_, instance))
    val triggered = requests ++ crashed
    NextState(copy(alive = Set.empty, detected = detected ++ crashedProcesses, triggers = triggers.trigger(triggered)), triggered)
  }

  def heartbeatReply(p: ProcessID): PFDState = copy(alive = alive + p)

  def heartbeatReq(send: Send): NextState[PFDState] = NextState(copy(triggers = triggers.trigger(send)), Set(send))

  def reply(send: Send): PFDState = copy(triggers = triggers.markAsTriggered(send))

  def request(send: Send): PFDState = copy(triggers = triggers.markAsTriggered(send))

  def crashed(action: Crashed): PFDState = copy(triggers = triggers.markAsTriggered(action))
}

case class PerfectFailureDetector(id: ProcessID, instance: InstanceID)(implicit val processes: Processes) extends Automaton[PFDState] {
  import oss.ggiussi.cappio.impl.Instances._
  val neighbors = processes.neighbors(id)

  override val sig: ActionSignature = {
    val in: Set[ActionHeader] = neighbors.map(DeliverHeader(_,id,FAILURE_DET_LINK)) ++ Set(Tick)  // TODO los delivers y send los podria tomar del link si tuviera composicion de automatas
    val out: Set[ActionHeader] = neighbors.map(SendHeader(id,_,FAILURE_DET_LINK)) ++ neighbors.map(Crashed(_,instance))
    val int: Set[ActionHeader] = Set(Timeout(instance))
    ActionSignature(in = in, out = out, int = int)
  }

  override val steps: Steps.Steps[PFDState] = Steps.steps2[PFDState](sig, {
    case Tick => Effect.triggers(_.tick())
    case Timeout(_) => Effect.triggers(_.shouldTimeout(), _.timeout())
    case Deliver(_,Message(HeartbeatRequest(q),_)) => Effect.triggers(_.heartbeatReq(Send(SendHeader(id,q,FAILURE_DET_LINK),Message(HeartbeatReply(id)))))
    case Deliver(_,Message(HeartbeatReply(p),_)) => Effect(_.heartbeatReply(p))

    // TODO estas tres son output actions y por lo tanto hacen lo mismo
    case s@Send(_,Message(HeartbeatRequest(_), _)) => Effect(_.triggers.wasTriggered(s), _.request(s))
    case s@Send(_,Message(HeartbeatReply(_), _)) => Effect(_.triggers.wasTriggered(s), _.reply(s))
    case s@Crashed(_, _) => Effect(_.triggers.wasTriggered(s), _.crashed(s))
  })
}
