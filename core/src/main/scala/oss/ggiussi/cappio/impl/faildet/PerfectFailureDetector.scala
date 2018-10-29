package oss.ggiussi.cappio.impl.faildet

import oss.ggiussi.cappio.core.LinkProtocol.{Deliver, Send}
import oss.ggiussi.cappio.{InstanceID, ProcessID}
import oss.ggiussi.cappio.core._
import oss.ggiussi.cappio.impl.faildet.PerfectFailureDetectorProtocol.{HeartbeatReply, HeartbeatRequest, Timeout}
import oss.ggiussi.cappio.impl.links.Message


object PerfectFailureDetectorProtocol {

  case class Timeout(instance: InstanceID) extends Action

  case class Crashed(p: ProcessID, instance: InstanceID) extends Action

  sealed trait Heartbeat

  case class HeartbeatRequest(q: ProcessID) extends Heartbeat

  case class HeartbeatReply(p: ProcessID) extends Heartbeat

}

object PFDState {
  def init(id: ProcessID)(implicit processes: Set[ProcessID]): PFDState = new PFDState(processes - id, Set.empty, Set.empty, Set.empty, id)
}

// TODO timer !
case class PFDState(alive: Set[ProcessID], detected: Set[ProcessID], repliesToSend: Set[ProcessID], requestsToSend: Set[ProcessID], id: ProcessID)(implicit processes: Set[ProcessID]) {
  val neighbors = processes - id

  def timeout(): PFDState = {
    //neighbors.filter(_ ) TODO
    copy(alive = Set.empty)
  }

  def heartbeatReply(p: ProcessID): PFDState = copy(alive = alive + p)

  def heartbeatReq(p: ProcessID): PFDState = copy(repliesToSend = repliesToSend + p)

  def canReply(to: ProcessID): Boolean = repliesToSend contains to

  def reply(to: ProcessID): PFDState = copy(repliesToSend = repliesToSend - to)

  def canReq(to: ProcessID): Boolean = requestsToSend contains to

  def request(to: ProcessID): PFDState = copy(requestsToSend = requestsToSend - to)
}

case class PerfectFailureDetector(id: ProcessID, instance: InstanceID)(implicit processes: Set[ProcessID]) extends Automaton[PFDState] {
  val neighbors = processes - id

  override val sig: ActionSignature = ???

  override val steps: Steps.Steps[PFDState] = Steps.steps[PFDState]({
    case Timeout(`instance`) => ???
    case Deliver(_, `id`, `instance`, Message(HeartbeatRequest(q), _)) => Effect(_.heartbeatReq(q))
    case Deliver(_, `id`, `instance`, Message(HeartbeatReply(p), _)) => Effect(_.heartbeatReply(p))
    case Send(`id`, to, `instance`, Message(HeartbeatReply(q), _)) => Effect(_.canReply(q), _.reply(q))
    case Send(`id`, to, `instance`, Message(HeartbeatRequest(p), _)) => ???
  })
}
