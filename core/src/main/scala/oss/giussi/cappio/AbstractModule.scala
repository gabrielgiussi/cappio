package oss.giussi.cappio

import oss.giussi.cappio.impl.net.FairLossLink.FLLSend
import oss.giussi.cappio.impl.net.Socket

import scala.annotation.tailrec
import scala.collection.immutable.Queue

// FIXME abstract class BaseState[R,S,I,Self <: StateWithModule[R,S,I,Self]](m: Module[R,S,I]) extends StateWithModule[R,S,I,Self]

trait StateWithModule[R, S, I, Self <: StateWithModule[R, S, I, Self]] {
  this: Self =>
  def updateModule(m: Module[R, S, I]): Self // FIXME como hago para que aca sea updateModule(m: A) donde A <: Module[R, S, I]

  def module(): Module[R, S, I]

  final def tail = module().tail
}

object Messages {

  sealed trait Message[+R, +UR, +US, +UI]

  sealed trait LocalMsg[+R, +UI] extends Message[R, Nothing, Nothing, Nothing]

  case class PublicRequest[R](r: R) extends LocalMsg[R, Nothing]

  case class LocalIndication[UI](r: UI) extends LocalMsg[Nothing, UI]

  case object Tick extends LocalMsg[Nothing, Nothing]

  sealed trait OtherMsg[+UR, +US, +UI] extends Message[Nothing, UR, US, UI]

  case object DelegateTick extends OtherMsg[Nothing, Nothing, Nothing]

  case class LocalNextState[UR, US, UI](ns: NextState[UR, US, UI]) extends OtherMsg[UR, US, UI]

  case class LocalRequest[UR](r: UR) extends OtherMsg[UR, Nothing, Nothing]

  object LocalStep {
    def withState[S, I, UR, UI](s: S): LocalStep[S, I, UR, UI] = LocalStep(Set.empty, Set.empty, Set.empty, Set.empty, s)

    def withRequests[S, I, UR, UI](requests: Set[LocalRequest[UR]], ns: S): LocalStep[S, I, UR, UI] = LocalStep(Set.empty, Set.empty, requests, Set.empty, ns)

    def withEvents[S, I, UR, UI](events: Set[LocalIndication[UI]], sends: Set[FLLSend], ns: S): LocalStep[S, I, UR, UI] = new LocalStep(Set.empty, events, Set.empty, sends, ns)

    def withIndications[S, I, UR, UI](indications: Set[I], ns: S): LocalStep[S, I, UR, UI] = new LocalStep(indications, Set.empty, Set.empty, Set.empty, ns)

    def withRequestsAndIndications[S, I, UR, UI](indications: Set[I], requests: Set[LocalRequest[UR]], ns: S): LocalStep[S, I, UR, UI] = new LocalStep(indications, Set.empty, requests, Set.empty, ns)

    def withModule[S <: StateWithModule[UR, US, UI, S], US, I, UR, UI](s: S, ns: NextState[UR, US, UI]): LocalStep[S, I, UR, UI] = {
      val indications = ns.indications.map(LocalIndication(_))
      new LocalStep(Set.empty,indications,Set.empty,ns.send, s.updateModule(ns.module))
    }

  }

  // o es mejor un sealed trait y los distintos tipos?
  case class LocalStep[S, I, UR, UI](indications: Set[I], events: Set[LocalIndication[UI]], requests: Set[LocalRequest[UR]], sends: Set[FLLSend], ns: S)

  type ProcessLocal[R, S, I, UR, UI] = (LocalMsg[R, UI],S) => LocalStep[S, I, UR, UI]

}

abstract class AbstractModule[R, S <: StateWithModule[UR, US, UI, S], I, UR, US, UI] extends Module[R, S, I] {

  import Messages._

  type Msg = Message[R, UR, US, UI]

  type LMsg = LocalMsg[R, UI]
  type LStep = LocalStep[S, I, UR, UI]
  type LReq = LocalRequest[UR]
  type LInd = LocalIndication[UI]
  type PLocal = ProcessLocal[R,S,I,UR,UI]

  override def request(in: R): Next = requestMsg(Seq(PublicRequest(in)))

  private def requestMsg(msgs: Seq[Msg]) = {
    val queue = Queue[Msg](msgs: _*)
    val (s, ind, sends) = processQueue(queue, state)
    //if (!ind.isEmpty) println(ind)
    next(copyModule(s), ind, sends)
  }

  def copyModule(state: S): AbstractModule[R, S, I, UR, US, UI]

  def processQueue(queue: Queue[Msg], state: S) = {
    @tailrec
    def pq(queue: Queue[Msg], indications: Set[I], sends: Set[FLLSend], state: S): (S, Set[I], Set[FLLSend]) = {
      if (queue.isEmpty) (state, indications, sends)
      else {
        // save all events and indications, it may be useful for UI! (e.g. PFD.Crashed)
        val LocalStep(ind, events, rqs, ss, ns) = processMsg(queue.head, state)
        // TODO watch out with the order of this events, if its matters I'm in trouble
        pq(queue.tail ++ events ++ rqs, indications ++ ind, sends ++ ss, ns)
      }
    }

    pq(queue, Set.empty, Set.empty, state)
  }

  def processMsg(msg: Msg, state: S): LStep = msg match {
    case l: LMsg => processLocal(l, state)
    case DelegateTick => LocalStep.withModule(state, state.module().tick)
    case LocalRequest(r) => LocalStep.withModule(state, state.module().request(r))
    case LocalNextState(ns) => LocalStep.withModule(state, ns)//LocalStep(events.map(LocalIndication(_)), sends, state.updateModule(module)) // TODO is needed si tengo el implicit def?
  }

  // or process(r: R) to avoid pattern match over PublicRequest, LocalEvent, etc.
  val processLocal: PLocal

  override def tail: Socket[R, S, I] = (packet: FLLDeliver) => requestMsg(Seq(LocalNextState(t.deliver(packet))))

  final def t: Socket[UR, US, UI] = state.tail

  override def tick: Next = requestMsg(Seq(Messages.Tick, DelegateTick)) // TODO order matters

}

case class CombinedModule[R1, S1, I1, R2, S2, I2, S](i1: Instance, m1: Module[R1, S1, I1], i2: Instance, m2: Module[R2, S2, I2], fs: (S1, S2) => S) extends Module[Either[R1, R2], S, Either[I1, I2]] {
  override def request(in: Either[R1, R2]): Next = in match {
    case Left(r) => p1(m1.request(r))
    case Right(r) => p2(m2.request(r))
  }

  private def p1(ns: NextState[R1, S1, I1]) = next(copy(m1 = ns.module), ns.indications.map(Left(_)), ns.send)

  private def p2(ns: NextState[R2, S2, I2]) = next(copy(m2 = ns.module), ns.indications.map(Right(_)), ns.send)

  override def state: S = fs(m1.state, m2.state)

  override def tail: Socket[Either[R1, R2], S, Either[I1, I2]] = (fll: FLLDeliver) => {
    if (fll.packet.instance == i1)
      p1(m1.tail.deliver(fll))
    else if (fll.packet.instance == i2) p2(m2.tail.deliver(fll))
    else throw new RuntimeException("No instance match")
  }

  override def tick: Next = {
    val NextState(ind1, send1, ns1) = m1.tick
    val NextState(ind2, send2, ns2) = m2.tick
    val lind = ind1.map(Left(_))
    val rind = ind2.map(Right(_))
    next(copy(m1 = ns1, m2 = ns2), lind ++ rind, send1 ++ send2)
  }
}