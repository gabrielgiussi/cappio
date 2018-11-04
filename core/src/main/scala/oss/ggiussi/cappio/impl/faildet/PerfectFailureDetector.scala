package oss.ggiussi.cappio.impl.faildet

import oss.ggiussi.cappio.core.LinkProtocol.{Deliver, Send}
import oss.ggiussi.cappio.{InstanceID, ProcessID}
import oss.ggiussi.cappio.core._
import oss.ggiussi.cappio.impl.Instances
import oss.ggiussi.cappio.impl.faildet.PerfectFailureDetectorProtocol._
import oss.ggiussi.cappio.impl.links.{Message, MessageID}


object PerfectFailureDetectorProtocol {

  case class Timeout(instance: InstanceID, step: Int) extends Action

  case class Crashed(p: ProcessID, instance: InstanceID) extends Action

  sealed trait Heartbeat

  case class HeartbeatRequest(q: ProcessID) extends Heartbeat

  case class HeartbeatReply(p: ProcessID) extends Heartbeat

  case class Heartbeats(steps: Int, id: ProcessID, neighbors: Set[ProcessID], instance: InstanceID) {

    private def message(f: (Int,ProcessID) => Action): Set[Action] = for (n <- neighbors; s <- 0 until steps) yield f(s,n)
    private def toSend(f: ProcessID => Heartbeat): Set[Action] = message((step,neigbor) => Send(id,neigbor,instance,Message(id,neigbor,f(id),step)))
    private def toDeliver(f: ProcessID => Heartbeat): Set[Action] = message((step,neigbor) => Deliver(neigbor,id,instance,Message(neigbor,id,f(neigbor),step)))

    def sends: Set[Action] = toSend(HeartbeatReply.apply) ++ toSend(HeartbeatRequest.apply)

    def delivers: Set[Action] = toDeliver(HeartbeatReply.apply) ++ toDeliver(HeartbeatRequest.apply)
  }

}

object PFDState {
  def init(id: ProcessID)(implicit processes: Set[ProcessID]): PFDState = new PFDState(processes - id, Set.empty, Set.empty, Set.empty, id)
}

// TODO timer !
case class PFDState(alive: Set[ProcessID], detected: Set[ProcessID], repliesToSend: Set[Send], requestsToSend: Set[Send], id: ProcessID)(implicit processes: Set[ProcessID]) {
  val neighbors = processes - id

  def timeout(timeout: Timeout): NextState[PFDState] = {
    val crashed = neighbors.filter(p => !(alive contains p) && !(detected contains p))
    val requests = neighbors.map(n => Send(id,n,Instances.FAILURE_DET_LINK,Message(id,n,HeartbeatRequest(id),timeout.step)))
    NextState(copy(alive = Set.empty, detected = detected ++ crashed, requestsToSend = requestsToSend ++ requests),crashed.map(Crashed(_,timeout.instance)) ++ requests)
  }

  def heartbeatReply(p: ProcessID): PFDState = copy(alive = alive + p)

  def heartbeatReq(send: Send): NextState[PFDState] = NextState(copy(repliesToSend = repliesToSend + send), Set(send))

  def canReply(send: Send): Boolean = repliesToSend contains send

  def reply(to: Send): PFDState = copy(repliesToSend = repliesToSend - to)

  def canReq(send: Send): Boolean = requestsToSend contains send

  def request(send: Send): PFDState = copy(requestsToSend = requestsToSend + send)
}

case class PerfectFailureDetector(id: ProcessID, instance: InstanceID)(implicit processes: Set[ProcessID], _steps: Int) extends Automaton[PFDState] {
  val neighbors = processes - id

  import oss.ggiussi.cappio.impl.Instances._

  override val sig: ActionSignature = {
    val heartbeats = Heartbeats(_steps,id,neighbors,FAILURE_DET_LINK)

    val in: Set[Action] = heartbeats.delivers
    val out: Set[Action] = heartbeats.sends
    val int: Set[Action] = (0 to _steps).map(Timeout(instance,_)).toSet // TODO va a estar siempre enabled
    ActionSignature(in = in, out = out, int = int)
  }

  override val steps: Steps.Steps[PFDState] = Steps.steps[PFDState]({
    case t@Timeout(`instance`,_) => Effect.triggers(_.timeout(t))
    case Deliver(_, `id`, `FAILURE_DET_LINK`, Message(HeartbeatRequest(q), MessageID(_, _, _, step))) => Effect.triggers(_.heartbeatReq(Send(Message(id, q, HeartbeatReply(id), step))(FAILURE_DET_LINK)))
    case Deliver(_, `id`, `FAILURE_DET_LINK`, Message(HeartbeatReply(p), _)) => Effect(_.heartbeatReply(p))

    case s@Send(`id`, _, `FAILURE_DET_LINK`, Message(HeartbeatRequest(`id`), _)) => Effect(_.canReq(s),_.request(s)) // triggerd action
    case s@Send(`id`, _, `FAILURE_DET_LINK`, Message(HeartbeatReply(`id`),_)) => Effect(_.canReply(s), _.reply(s)) // triggered action
  })
}

object PFD extends App {

  val i = InstanceID("pfd")

  val automaton = PerfectFailureDetector(0,i)(Set(1,2),5)

  val state = PFDState.init(0)(Set(1,2))

  val execution = Execution(automaton,state)

  println(execution
    .next(Deliver(1,0,Instances.FAILURE_DET_LINK,Message(1,0,HeartbeatRequest(1),0)))
    .next(Timeout(i,0))
    .next(Deliver(1,0,Instances.FAILURE_DET_LINK,Message(0,1,HeartbeatReply(1),0)))
    .sched())

}