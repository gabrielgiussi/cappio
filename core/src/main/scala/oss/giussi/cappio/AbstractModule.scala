package oss.giussi.cappio

import oss.giussi.cappio.impl.net.FairLossLink.FLLSend
import oss.giussi.cappio.impl.net.Socket

import scala.annotation.tailrec
import scala.collection.immutable.Queue

// FIXME abstract class BaseState[R,S,I,Self <: StateWithModule[R,S,I,Self]](m: Module[R,S,I]) extends StateWithModule[R,S,I,Self]

trait StateWithModule[R, S, I, Self <: StateWithModule[R,S,I,Self]] { this: Self =>
  def updateModule(m: Module[R, S, I]): Self // FIXME como hago para que aca sea updateModule(m: A) donde A <: Module[R, S, I]

  def module(): Module[R,S,I]

  final def tail = module().tail
}

abstract class AbstractModule[R, S <: StateWithModule[UR, US, UI, S], I, UR, US, UI] extends Module[R, S, I] {
  sealed trait Msg

  sealed trait LocalMsg extends Msg
  case class PublicRequest(r: R) extends LocalMsg
  case class LocalIndication(r: UI) extends LocalMsg
  case object Tick extends LocalMsg

  case object DelegateTick extends Msg
  case class LocalNextState(ns: NextState[UR,US,UI]) extends Msg
  case class LocalRequest(r: UR) extends Msg

  object LocalStep {
    def apply(s: S): LocalStep = LocalStep(Set.empty,Set.empty,Set.empty,Set.empty,s)

    def localRequest(requests: Set[LocalRequest], ns: S): LocalStep = LocalStep(Set.empty,Set.empty,requests,Set.empty,ns)

    def apply(events: Set[LocalIndication], sends: Set[FLLSend], ns: S): LocalStep = new LocalStep(Set.empty, events, Set.empty, sends, ns)

    def apply(indications: Set[I], ns: S): LocalStep = new LocalStep(indications,Set.empty,Set.empty,Set.empty, ns)

    // rename
    def prueba(indications: Set[I], requests: Set[LocalRequest], ns: S): LocalStep = new LocalStep(indications, Set.empty, requests, Set.empty, ns)

    // TODO CUIDADO CON USAR state aca porque lo tengo accesible pero es el state "viejo". No deberia estar visible para evitar errores
    def apply(s: S, ns: NextState[UR,US,UI]): LocalStep = LocalStep(ns.indications.map(LocalIndication(_)), ns.send, s.updateModule(ns.module))
  }

  // if indications can be mixed with events it will be better to separate in two classes
  // ns should be an Option[S] asi puedo decir q no cambio el estado
  case class LocalStep(indications: Set[I], events: Set[LocalIndication],requests: Set[LocalRequest], sends: Set[FLLSend], ns: S)

  //implicit def usesNextStateAsLocalStep(ns: NextState[UR,US,UI]): LocalStep = LocalStep(ns.indications.map(LocalIndication), ns.send, state.updateModule(ns.module))

  override def request(in: R): Next = requestMsg(Seq(PublicRequest(in)))

  private def requestMsg(msgs: Seq[Msg]) = {
    val queue = Queue[Msg](msgs : _*)
    val (s,ind,sends) = processQueue(queue, state)
    //if (!ind.isEmpty) println(ind)
    next(copyModule(s),ind,sends)
  }

  def copyModule(state: S): AbstractModule[R,S,I,UR,US,UI]

  def processQueue(queue: Queue[Msg], state: S) = {
    @tailrec
    def pq(queue: Queue[Msg], indications: Set[I], sends: Set[FLLSend], state: S): (S, Set[I], Set[FLLSend]) = {
      if (queue.isEmpty) (state, indications, sends)
      else {
        // save all events and indications, it may be useful for UI! (e.g. PFD.Crashed)
        val LocalStep(ind, events, rqs,ss, ns) = processMsg(queue.head, state)
        // TODO watch out with the order of this events, if its matters I'm in trouble
        pq(queue.tail ++ events ++ rqs, indications ++ ind, sends ++ ss, ns)
      }
    }

    pq(queue, Set.empty, Set.empty, state)
  }

  def processMsg(msg: Msg, state: S): LocalStep = msg match {
    case DelegateTick => LocalStep(state,state.module().tick)
    case l: LocalMsg => processLocal(l, state)
    case LocalRequest(r) => LocalStep(state,state.module().request(r))
    case LocalNextState(NextState(events, sends, module: Module[UR, US, UI])) => LocalStep(events.map(LocalIndication(_)), sends, state.updateModule(module)) // TODO is needed si tengo el implicit def?
  }

  // or process(r: R) to avoid pattern match over PublicRequest, LocalEvent, etc.
  def processLocal(l: LocalMsg, state: S): LocalStep

  override def tail: Socket[R, S, I] = (packet: FLLDeliver) => requestMsg(Seq(LocalNextState(t.deliver(packet))))

  final def t: Socket[UR,US,UI] = state.tail

  override def tick: Next = requestMsg(Seq(Tick,DelegateTick)) // TODO order matters

}

case class CombinedModule[R1,S1,I1,R2,S2,I2,S](i1: Instance, m1: Module[R1,S1,I1], i2: Instance, m2: Module[R2,S2,I2], fs: (S1,S2) => S) extends Module[Either[R1,R2],S,Either[I1,I2]] {
  override def request(in: Either[R1, R2]): Next = in match {
    case Left(r) => p1(m1.request(r))
    case Right(r) => p2(m2.request(r))
  }

  private def p1(ns: NextState[R1,S1,I1]) = next(copy(m1 = ns.module), ns.indications.map(Left(_)), ns.send)

  private def p2(ns: NextState[R2,S2,I2]) = next(copy(m2 = ns.module), ns.indications.map(Right(_)), ns.send)

  override def state: S = fs(m1.state,m2.state)

  override def tail: Socket[Either[R1, R2], S, Either[I1, I2]] = (fll: FLLDeliver) => {
    if (fll.packet.instance == i1)
      p1(m1.tail.deliver(fll))
    else if (fll.packet.instance == i2) p2(m2.tail.deliver(fll))
    else throw new RuntimeException("No instance match")
  }

  override def tick: Next = {
    val NextState(ind1,send1,ns1) = m1.tick
    val NextState(ind2,send2,ns2) = m2.tick
    val lind = ind1.map(Left(_))
    val rind = ind2.map(Right(_))
    next(copy(m1 = ns1, m2 = ns2), lind ++ rind, send1 ++ send2)
  }
}