package oss.giussi.cappio

import oss.giussi.cappio.Messages.{LocalIndication, LocalMsg, LocalRequest, LocalStep, ProcessLocalM, PublicRequest, Tick}
import oss.giussi.cappio.impl.net.FairLossLink.FLLSend
import oss.giussi.cappio.impl.net.Socket
import shapeless.ops.coproduct.Inject
import shapeless.{:+:, CNil, Coproduct, Inl, Inr}

import scala.annotation.tailrec
import scala.collection.immutable.Queue

trait ModS[M <: Mod] extends Mod {
  type S <: StateWithModule[M, S]
  final override type State = S
  final override type Payload = M#Payload
  type Dep = M

}

// THIS MUST DIE!
trait StateWithModule[M <: Mod, Self <: StateWithModule[M, Self]] {
  this: Self =>
  def updateModule(m: Module[M]): Self

  def module: Module[M]

  final def tail = module.tail
}

case class BasicState[M <: Mod](module: Module[M]) extends StateWithModule[M,BasicState[M]]{
  override def updateModule(m: Module[M]): BasicState[M] = copy(m)
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
    def withState[S, I, UR, UI,P](s: S): LocalStep[S, I, UR, UI,P] = LocalStep(Set.empty, Set.empty, Set.empty, Set.empty, s)

    def withRequests[S, I, UR, UI,P](requests: Set[LocalRequest[UR]], ns: S): LocalStep[S, I, UR, UI,P] = LocalStep(Set.empty, Set.empty, requests, Set.empty, ns)

    def withEvents[S, I, UR, UI,P](events: Set[LocalIndication[UI]], sends: Set[FLLSend[P]], ns: S): LocalStep[S, I, UR, UI,P] = new LocalStep(Set.empty, events, Set.empty, sends, ns)

    def withIndications[S, I, UR, UI,P](indications: Set[I], ns: S): LocalStep[S, I, UR, UI,P] = new LocalStep(indications, Set.empty, Set.empty, Set.empty, ns)

    def withRequestsAndIndications[S, I, UR, UI,P](indications: Set[I], requests: Set[LocalRequest[UR]], ns: S): LocalStep[S, I, UR, UI,P] = new LocalStep(indications, Set.empty, requests, Set.empty, ns)

    def withModule[I, S <: StateWithModule[M, S], M <: Mod](s: S, ns: NextState[M]): LocalStep[S, I, M#Req, M#Ind,M#Payload] = {
      val indications = ns.indications.map(LocalIndication(_))
      new LocalStep(Set.empty, indications, Set.empty, ns.send, s.updateModule(ns.module))
    }

  }

  // o es mejor un sealed trait y los distintos tipos?
  case class LocalStep[S, I, UR, UI,P](indications: Set[I], events: Set[LocalIndication[UI]], requests: Set[LocalRequest[UR]], sends: Set[FLLSend[P]], ns: S)

  type ProcessLocal[R, S, I, UR, UI,P] = (LocalMsg[R, UI], S) => LocalStep[S, I, UR, UI,P]

  type ProcessLocalM[M <: ModS[Dep],Dep <: Mod] = ProcessLocal[M#Req,M#State,M#Ind,Dep#Req,Dep#Ind, Dep#Payload]

}

trait ProcessLocalHelper1[M <: ModS[Dep], Dep <: Mod] extends Function2[LocalMsg[M#Req,Dep#Ind],M#State,LocalStep[M#State,M#Ind,Dep#Req,Dep#Ind,Dep#Payload]] {

  type Output = LocalStep[M#State,M#Ind,Dep#Req,Dep#Ind,Dep#Payload]
  type State = M#State // Or M#S?
  type DInd = Dep#Ind

  final override def apply(msg: LocalMsg[M#Req, Dep#Ind], state: State): Output = msg match {
    case PublicRequest(req) => onPublicRequest(req,state)
    case LocalIndication(ind) => onIndication(ind,state)
    case Tick => onTick(state)
  }

  def onPublicRequest(req: M#Req, state: State): Output

  def onIndication(ind: DInd, state: State): Output

  def onTick(state: State): Output = LocalStep.withState(state)
}

// como evitar estos injectors?
abstract class ProcessLocalHelper2[M <: ModS[Dep], Dep <: Mod2](implicit inj1: Inject[Dep#Req, Dep#Dep1#Req], inj2: Inject[Dep#Req, Dep#Dep2#Req]) extends ProcessLocalHelper1[M,Dep] {
  import shapeless.Coproduct


  override def onIndication(ind: DInd, state: State): Output = ind match {
    case Inl(ind) => onDependencyIndication1(ind,state)
    case Inr(Inl(ind)) => onDependencyIndication2(ind,state)
    case Inr(Inr(_)) => LocalStep.withState(state)
  }

  def onDependencyIndication1(ind: Dep#Dep1#Ind, state: State): Output

  def onDependencyIndication2(ind: Dep#Dep2#Ind, state: State): Output

  def req1(r: Dep#Dep1#Req) : LocalRequest[Dep#Req] = LocalRequest(Coproduct[Dep#Req](r)(inj1))

  // una opcion es que el LocalStep tenga un apply q haga estas conversiones asi no tengo que usar este metodo
  def req2(r: Dep#Dep2#Req): LocalRequest[Dep#Req] = LocalRequest(Coproduct[Dep#Req](r)(inj2))
}

object AbstractModule {

  def mod[M <: ModS[Dep], Dep <: Mod](initial: M#S, p: ProcessLocalM[M,Dep]): Module[M] = {
    case class InternalModule(s: M#S) extends AbstractModule[M,Dep]{
      override def copyModule(state: M#S): AbstractModule[M, Dep] = copy(state)

      override val processLocal: PLocal = p

      override def state: M#S = s
    }
    InternalModule(initial)
  }
}

trait AbstractModule[M1 <: ModS[M2], M2 <: Mod] extends Module[M1] {

  import Messages._

  final type R = M1#Req
  final type UR = M2#Req
  final type S = M1#State
  final type US = M2#State
  final type I = M1#Ind
  final type UI = M2#Ind
  final type P = M2#Payload

  final type Msg = Message[R, UR, US, UI]

  final type LMsg = LocalMsg[R, UI]
  final type LStep = LocalStep[S, I, UR, UI,P]
  final type LReq = LocalRequest[UR]
  final type LInd = LocalIndication[UI]
  final type PLocal = ProcessLocal[R, S, I, UR, UI,P]

  final override def request(in: R): Next = requestMsg(Seq(PublicRequest(in)))

  final private def requestMsg(msgs: Seq[Msg]) = {
    val queue = Queue[Msg](msgs: _*)
    val (s, ind, sends) = processQueue(queue, state)
    //if (!ind.isEmpty) println(ind)
    next(copyModule(s), ind, sends)
  }

  def copyModule(state: S): AbstractModule[M1, M2]

  final private def processQueue(queue: Queue[Msg], state: S) = {
    @tailrec
    def pq(queue: Queue[Msg], indications: Set[I], sends: Set[FLLSend[P]], state: S): (S, Set[I], Set[FLLSend[P]]) = {
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

  final def processMsg(msg: Msg, state: S): LStep = msg match {
    case l: LMsg => processLocal(l, state)
    case DelegateTick => withModule(state, state.module.tick)
    case LocalRequest(r) => withModule(state, state.module.request(r))
    case LocalNextState(ns) => withModule(state, ns)
  }

  // or process(r: R) to avoid pattern match over PublicRequest, LocalEvent, etc.
  val processLocal: PLocal


  final override def tail: Socket[M1] = (packet: FLLDeliver[P]) => requestMsg(Seq(LocalNextState(t.deliver(packet))))

  final def t: Socket[M2] = state.tail

  final override def tick: Next = requestMsg(Seq(Messages.Tick, DelegateTick)) // TODO order matters

}

object CombinedModule {
  def paired[M1 <: Mod, M2 <: Mod](i1: Instance, m1: Module[M1], i2: Instance, m2: Module[M2]) = new CombinedModule[M1,M2, (M1#State, M2#State)](i1, m1, i2, m2, Tuple2.apply)

}

trait Mod2 extends Mod {
  type Dep1 <: Mod
  type Dep2 <: Mod
  final override type Req = Dep1#Req :+: Dep2#Req :+: CNil
  final override type Ind = Dep1#Ind :+: Dep2#Ind :+: CNil
  final override type Payload = Dep1#Payload :+: Dep2#Payload :+: CNil
}

// lo puedo hacer q se banque N modulos con HList?
// No estoy usando las instances!
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
    val send = ns.send.map { case FLLSend(p) => FLLSend(p.copy(payload = Coproduct[SelfMod#Payload](p.payload))) } // TODO
    next(i, ns.indications.map(Coproduct[SelfMod#Ind](_)), send)
  }

  private def p2(ns: NextState[M2]): Next = {
    val i: Self = copy(m2 = ns.module)
    val send = ns.send.map { case FLLSend(p) => FLLSend(p.copy(payload = Coproduct[SelfMod#Payload](p.payload))) }
    next(i, ns.indications.map(Coproduct[SelfMod#Ind](_)), send) // TODO duplicated code
  }

  override def state: S = fs(m1.state, m2.state)

  override def tail: Socket[SelfMod] = (d: FLLDeliver[SelfMod#Payload]) => d.packet match {
    case p@Packet(_, Inl(payload), _, _, _) => p1(m1.tail.deliver(FLLDeliver(p.copy(payload = payload))))
    case p@Packet(_, Inr(Inl(payload)), _, _, _) => p2(m2.tail.deliver(FLLDeliver(p.copy(payload = payload))))
    case _ => throw new RuntimeException("can't happen")
  }


  override def tick: Next = {
    val NextState(ind1, send1, ns1) = m1.tick
    val NextState(ind2, send2, ns2) = m2.tick
    val lind = ind1.map(Coproduct[SelfMod#Ind](_))
    val rind = ind2.map(Coproduct[SelfMod#Ind](_))
    val s: Self = copy(m1 = ns1, m2 = ns2)
    val s1 = send1.map { case FLLSend(p@Packet(_, payload, _, _, _)) => FLLSend(p.copy(payload = Coproduct[SelfMod#Payload](payload))) }
    val s2 = send2.map { case FLLSend(p@Packet(_, payload, _, _, _)) => FLLSend(p.copy(payload = Coproduct[SelfMod#Payload](payload))) }
    next(s, lind ++ rind, s1 ++ s2)
  }

  object req extends shapeless.Poly1 {
    implicit val request1: Case.Aux[M1#Req, Next] = at(r => p1(m1.request(r)))

    implicit val request2: Case.Aux[M2#Req, Next] = at(r => p2(m2.request(r)))
  }

  override def request(in: SelfMod#Req): Next = in.fold(req)

}