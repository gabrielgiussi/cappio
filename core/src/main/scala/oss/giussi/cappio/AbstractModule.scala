package oss.giussi.cappio

import oss.giussi.cappio.impl.net.FairLossLink.FLLSend
import oss.giussi.cappio.impl.net.Socket
import shapeless.{:+:, CNil, Coproduct}

import scala.annotation.tailrec
import scala.collection.immutable.Queue

trait ModS[M <: Mod] extends Mod {
  type S <: StateWithModule[M, S]
  final override type State = S

}

// THIS MUST DIE!
trait StateWithModule[M <: Mod, Self <: StateWithModule[M, Self]] {
  this: Self =>
  def updateModule(m: Module[M]): Self

  def module: Module[M]

  final def tail = module.tail
}

object Messages {

  sealed trait Message[+R, +UR, +US, +UI]

  sealed trait LocalMsg[+R, +UI] extends Message[R, Nothing, Nothing, UI]

  case class PublicRequest[R](r: R) extends LocalMsg[R, Nothing]

  case class LocalIndication[UI](r: UI) extends LocalMsg[Nothing, UI]

  case object Tick extends LocalMsg[Nothing, Nothing]

  sealed trait OtherMsg[+UR, +US, +UI] extends Message[Nothing, UR, US, UI]

  case object DelegateTick extends OtherMsg[Nothing, Nothing, Nothing]

  case class LocalNextState[M <: Mod](ns: NextState[M]) extends OtherMsg[M#Req, M#State, M#Ind]

  case class LocalRequest[UR](r: UR) extends OtherMsg[UR, Nothing, Nothing]

  object LocalStep {
    def withState[S, I, UR, UI](s: S): LocalStep[S, I, UR, UI] = LocalStep(Set.empty, Set.empty, Set.empty, Set.empty, s)

    def withRequests[S, I, UR, UI](requests: Set[LocalRequest[UR]], ns: S): LocalStep[S, I, UR, UI] = LocalStep(Set.empty, Set.empty, requests, Set.empty, ns)

    def withEvents[S, I, UR, UI](events: Set[LocalIndication[UI]], sends: Set[FLLSend], ns: S): LocalStep[S, I, UR, UI] = new LocalStep(Set.empty, events, Set.empty, sends, ns)

    def withIndications[S, I, UR, UI](indications: Set[I], ns: S): LocalStep[S, I, UR, UI] = new LocalStep(indications, Set.empty, Set.empty, Set.empty, ns)

    def withRequestsAndIndications[S, I, UR, UI](indications: Set[I], requests: Set[LocalRequest[UR]], ns: S): LocalStep[S, I, UR, UI] = new LocalStep(indications, Set.empty, requests, Set.empty, ns)

    def withModule[I, S <: StateWithModule[M, S], M <: Mod](s: S, ns: NextState[M]): LocalStep[S, I, M#Req, M#Ind] = {
      val indications = ns.indications.map(LocalIndication(_))
      new LocalStep(Set.empty, indications, Set.empty, ns.send, s.updateModule(ns.module))
    }

  }

  // o es mejor un sealed trait y los distintos tipos?
  case class LocalStep[S, I, UR, UI](indications: Set[I], events: Set[LocalIndication[UI]], requests: Set[LocalRequest[UR]], sends: Set[FLLSend], ns: S)

  type ProcessLocal[R, S, I, UR, UI] = (LocalMsg[R, UI], S) => LocalStep[S, I, UR, UI]

}

trait AbstractModule[M1 <: ModS[M2], M2 <: Mod] extends Module[M1] {

  import Messages._

  type R = M1#Req
  type UR = M2#Req
  type S = M1#State
  type US = M2#State
  type I = M1#Ind
  type UI = M2#Ind

  type Msg = Message[R, UR, US, UI]

  type LMsg = LocalMsg[R, UI]
  type LStep = LocalStep[S, I, UR, UI]
  type LReq = LocalRequest[UR]
  type LInd = LocalIndication[UI]
  type PLocal = ProcessLocal[R, S, I, UR, UI]

  override def request(in: R): Next = requestMsg(Seq(PublicRequest(in)))

  private def requestMsg(msgs: Seq[Msg]) = {
    val queue = Queue[Msg](msgs: _*)
    val (s, ind, sends) = processQueue(queue, state)
    //if (!ind.isEmpty) println(ind)
    next(copyModule(s), ind, sends)
  }

  def copyModule(state: S): AbstractModule[M1, M2]

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

  private def withModule(s: S, ns: NextState[M2]): LStep = {
    val indications = ns.indications.map(LocalIndication(_))
    new LocalStep(Set.empty, indications, Set.empty, ns.send, s.updateModule(ns.module))
  }

  def processMsg(msg: Msg, state: S): LStep = msg match {
    case l: LMsg => processLocal(l, state)
    case DelegateTick => withModule(state, state.module.tick)
    case LocalRequest(r) => withModule(state, state.module.request(r))
    case LocalNextState(ns) => withModule(state, ns) //LocalStep(events.map(LocalIndication(_)), sends, state.updateModule(module)) // TODO is needed si tengo el implicit def?
  }

  // or process(r: R) to avoid pattern match over PublicRequest, LocalEvent, etc.
  val processLocal: PLocal

  override def tail: Socket[M1] = (packet: FLLDeliver) => requestMsg(Seq(LocalNextState(t.deliver(packet))))

  final def t: Socket[M2] = state.tail

  override def tick: Next = requestMsg(Seq(Messages.Tick, DelegateTick)) // TODO order matters

}

object CombinedModule {
  def paired[M1 <: Mod, M2 <: Mod](i1: Instance, m1: Module[M1], i2: Instance, m2: Module[M2]) = new CombinedModule[M1, M2, (M1#State, M2#State)](i1, m1, i2, m2, Tuple2.apply)

}

trait Mod2 extends Mod {
  type Dep1 <: Mod
  type Dep2 <: Mod
  final override type Req = Dep1#Req :+: Dep2#Req :+: CNil
  final override type Ind = Dep1#Ind :+: Dep2#Ind :+: CNil
}

// lo puedo hacer q se banque N modulos con HList?
case class CombinedModule[M1 <: Mod, M2 <: Mod, S](i1: Instance, m1: Module[M1], i2: Instance, m2: Module[M2], fs: (M1#State, M2#State) => S) extends Module[Mod2 {
  type Dep1 = M1
  type Dep2 = M2
  type State = S
}] {

  type SelfMod = Mod2 {
    type Dep1 = M1
    type Dep2 = M2
    type State = S
  }

  type Self = Module[SelfMod]

  private def p1(ns: NextState[M1]): Next = {
    val i: Self = copy(m1 = ns.module)
    next(i, ns.indications.map(Coproduct[SelfMod#Ind](_)), ns.send)
  }

  private def p2(ns: NextState[M2]): Next = {
    val i: Self = copy(m2 = ns.module)
    next(i, ns.indications.map(Coproduct[SelfMod#Ind](_)), ns.send) // TODO duplicated code
  }

  override def state: S = fs(m1.state, m2.state)

  override def tail: Socket[SelfMod] = (packet: FLLDeliver) => {
    if (packet.packet.instance == i1)
      p1(m1.tail.deliver(packet))
    else if (packet.packet.instance == i2) p2(m2.tail.deliver(packet))
    else throw new RuntimeException("No instance match")
  }

  override def tick: Next = {
    val NextState(ind1, send1, ns1) = m1.tick
    val NextState(ind2, send2, ns2) = m2.tick
    val lind = ind1.map(Coproduct[SelfMod#Ind](_))
    val rind = ind2.map(Coproduct[SelfMod#Ind](_))
    val s: Self = copy(m1 = ns1, m2 = ns2)
    next(s, lind ++ rind, send1 ++ send2)
  }

  object req extends shapeless.Poly1 {
    implicit val request1: Case.Aux[M1#Req, Next] = at(r => p1(m1.request(r)))

    implicit val request2: Case.Aux[M2#Req, Next] = at(r => p2(m2.request(r)))
  }

  // https://stackoverflow.com/questions/34107849/pattern-matching-with-shapeless-coproduct
  override def request(in: SelfMod#Req): Next = in.fold(req)
}