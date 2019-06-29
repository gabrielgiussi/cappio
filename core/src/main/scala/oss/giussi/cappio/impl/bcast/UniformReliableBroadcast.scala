package oss.giussi.cappio.impl.bcast

import java.util.UUID

import oss.giussi.cappio.Messages.ProcessLocal
import oss.giussi.cappio.impl.bcast.BestEffortBroadcast.{BEBState, BebBcast, BebDeliver}
import oss.giussi.cappio.impl.bcast.UniformReliableBroadcast._
import oss.giussi.cappio.impl.time.PerfectFailureDetector
import oss.giussi.cappio.impl.time.PerfectFailureDetector.{Crashed, PFDState}
import oss.giussi.cappio._

import scala.util.Random

object UniformReliableBroadcast {

  type ModuleReq = Either[NoRequest,BebBcast]

  type ModuleState = (PFDState,BEBState)

  type ModuleInd = Either[Crashed,BebDeliver]

  type Acks = Map[UUID,Set[ProcessId]]

  case class URBData(sender: ProcessId, msg: Any)

  // TODO
  val BEB = Instance("beb")
  val PFD = Instance("pfd")

  object URBState {
    def init(self: ProcessId, all: Set[ProcessId], timeout: Int) = {
      val pfdMod = PerfectFailureDetector.init(self,all,timeout) // Este timeout deberia ser enviado en un metodo Init al comienzo asi todos tienen el mismo!
      val bebMod = BestEffortBroadcast.init(self,all,timeout)
      val s = (s1: PFDState, s2: BEBState) => (s1,s2)
      val combined: Module[ModuleReq,ModuleState,ModuleInd] = CombinedModule(PFD,pfdMod,BEB,bebMod,s)
      URBState(Set.empty,Set.empty,all,Map.empty,combined)
    }
  }

  case class URBState(delivered: Set[UUID], pending: Set[(ProcessId,UUID,Any)],correct: Set[ProcessId], acks: Acks, module: Module[ModuleReq,ModuleState,ModuleInd]) extends StateWithModule[ModuleReq,ModuleState,ModuleInd,URBState] {
    override def updateModule(m: Module[ModuleReq, (PFDState, BEBState), ModuleInd]): URBState = copy(module = m)

    private def canDeliver(m: UUID): Boolean = correct -- acks(m) isEmpty

    def addPending(from: ProcessId, id: UUID, msg: Any) = copy(pending = pending + Tuple3(from, id, msg))

    def crashed(p: ProcessId) = copy(correct = correct - p)

    def ack(from: ProcessId, sender: ProcessId, id: UUID, msg: Any): (URBState,Option[(ProcessId,UUID,Any)]) = {
      val nacks = acks.updated(id,acks.get(id).map(_ + from).getOrElse(Set(from)))
      val (np,triggers) = {
        val tuple = (sender,id,msg)
        if (!pending.contains(tuple)) (pending + tuple, Some(tuple))
        else (pending,None)
      }
      (copy(acks = nacks, pending = np), triggers)
    }

    // TODO N indications o 1 indication?
    // tendria que inventar un evento que para evaluar conditions si quiero disparar todos, porque el trigger afecta el estado.
    // o agregar logica para que elimine duplicados por payload.id
    def evaluateCondition(): Option[(URBState,(ProcessId,Any))] = {
      val candidates = pending.filter(p => canDeliver(p._2) && !delivered.contains(p._2)).toList // random element
      if (candidates.isEmpty) None
      else {
        val (sender, id, msg) = candidates(Random.nextInt(candidates.length))
        Some((copy(delivered = delivered + id), (sender, msg)))
      }
    }
  }

  // TODO move class
  object Payload {
    def apply(msg: Any): Payload = new Payload(UUID.randomUUID(),msg)
  }

  // TODO porque necesitaba poner el id aca en lugar de en el Packet?

  case class Payload(id: UUID, msg: Any) // Payload[T](id: UUID, msg: T) ??

  case class URBBcast(payload: Payload)

  case class URBDeliver(from: ProcessId, payload: Any)

  def init(self: ProcessId, all: Set[ProcessId], timeout: Int) = UniformReliableBroadcast(self,URBState.init(self,all,timeout))

  def processLocal(self: ProcessId): ProcessLocal[URBBcast,URBState,URBDeliver,ModuleReq,ModuleInd] = {
    import Messages._
    (msg,state) => msg match {
      case PublicRequest(URBBcast(p)) =>
        val req: Set[LocalRequest[ModuleReq]] = Set(LocalRequest(Right(BebBcast(Payload(p.id,URBData(self,p.msg)),BEB))))
        LocalStep.withRequests(req,state.addPending(self, p.id,p.msg))
      case LocalIndication(Left(Crashed(id))) => LocalStep.withState(state.crashed(id))
      case LocalIndication(Right(BebDeliver(from, Payload(id,URBData(sender,msg))))) =>
        val (ns,triggers) = state.ack(from,sender,id,msg)
        val req: Set[LocalRequest[ModuleReq]] = triggers.map { case (pid,uuid,msg) => LocalRequest[ModuleReq](Right(BebBcast(Payload(uuid,URBData(pid,msg)),BEB))) }.toSet // aca el Self se lo va a estar poniendo la abstracion PerfectLink
        LocalStep.withRequests(req,ns)
      case Tick =>
        state.evaluateCondition() match {
          case Some((ns,(sender,msg))) => LocalStep.withIndications(Set(URBDeliver(sender,msg)),ns)
          case None => LocalStep.withState(state)
        }
      case LocalIndication(_) => LocalStep.withState(state)
    }
  }
}

case class UniformReliableBroadcast(self: ProcessId, state: URBState) extends AbstractModule[URBBcast,URBState,URBDeliver,ModuleReq,ModuleState,ModuleInd] {
  import UniformReliableBroadcast._
  import Messages._

  override def copyModule(s: URBState): AbstractModule[URBBcast, URBState, URBDeliver, ModuleReq, (PFDState, BEBState), ModuleInd] = copy(state = s)

  override val processLocal: PLocal = UniformReliableBroadcast.processLocal(self)
}
