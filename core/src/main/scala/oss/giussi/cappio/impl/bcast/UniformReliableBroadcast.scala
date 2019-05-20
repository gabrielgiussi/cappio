package oss.giussi.cappio.impl.bcast

import java.util.UUID

import oss.giussi.cappio.impl.bcast.BestEffortBroadcast.{BEBState, BebBcast, BebDeliver}
import oss.giussi.cappio.impl.bcast.UniformReliableBroadcast._
import oss.giussi.cappio.impl.net.Socket
import oss.giussi.cappio.impl.time.PerfectFailureDetector
import oss.giussi.cappio.impl.time.PerfectFailureDetector.{Crashed, PFDState}
import oss.giussi.cappio._

import scala.util.Random

object UniformReliableBroadcast {

  type ModuleReq = Either[NoRequest,BebBcast]

  type ModuleState = (PFDState,BEBState)

  type ModuleInd = Either[Crashed,BebDeliver]

  type Acks = Map[UUID,Set[ProcessId]]

  case class URBDeliverMsg(sender: ProcessId, msg: Any)

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

  object Payload {
    def apply(msg: Any): Payload = new Payload(UUID.randomUUID(),msg)
  }

  case class Payload(id: UUID, msg: Any)

  case class URBBcast(payload: Payload)

  case class URBDeliver(from: ProcessId, payload: Any)

  def init(self: ProcessId, all: Set[ProcessId], timeout: Int) = UniformReliableBroadcast(self,URBState.init(self,all,timeout))
}

case class UniformReliableBroadcast(self: ProcessId, state: URBState) extends AbstractModule[URBBcast,URBState,URBDeliver,ModuleReq,ModuleState,ModuleInd] {
  import UniformReliableBroadcast._

  override def copyModule(s: URBState): AbstractModule[URBBcast, URBState, URBDeliver, ModuleReq, (PFDState, BEBState), ModuleInd] = copy(state = s)

  override def processLocal(l: LocalMsg, state: URBState): LocalStep = l match {
    case PublicRequest(URBBcast(p)) =>
      val req = Set(LocalRequest(Right(BebBcast(Payload(p.id,URBDeliverMsg(self,p.msg)),BEB))))
      LocalStep.localRequest(req,state.addPending(self, p.id,p.msg))
    case LocalIndication(Left(Crashed(id))) => LocalStep(state.crashed(id))
    case LocalIndication(Right(BebDeliver(from, Payload(id,URBDeliverMsg(sender,msg))))) =>
      val (ns,triggers) = state.ack(from,sender,id,msg)
      val req = triggers.map(m => LocalRequest(Right(BebBcast(m._2,BEB)))).toSet // aca el Self se lo va a estar poniendo la abstracion PerfectLink
      LocalStep.localRequest(req,ns)
    case Tick =>
      state.evaluateCondition() match {
        case Some((ns,(sender,msg))) => LocalStep(Set(URBDeliver(sender,msg)),ns)
        case None => LocalStep(state)
      }

  }

  override def t: Socket[ModuleReq, (PFDState, BEBState), ModuleInd] = state.module.tail

}
