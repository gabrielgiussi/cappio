package oss.ggiussi.cappio.core

import oss.ggiussi.cappio.core.Transition.Transition
import oss.ggiussi.cappio.impl.links.Protocol.ProcessID
import oss.ggiussi.cappio.core.Execution.Triggers
import oss.ggiussi.cappio.core.FLLProtocol.{Deliver, Message, Send}
import oss.ggiussi.cappio.core.Steps.Steps
import oss.ggiussi.cappio.core.Transition.Transition

// TODO rename
case object UnsatisfiedPreconditionException extends RuntimeException

case object UnknownActionException extends RuntimeException

trait Action

object Effect {
  // // or Try[S] TODO
  // enrealidad precondition y f dependen del payload del action tmb, pero lo estoy capturando en un closure (es menos prolijo pero me deja escribir menos verbose)
  //def inputEnabled[S](f: S => S): Effect[S] = new Effect((_, _) => true, { case (s, _) => f(s) })

  //def inputEnabledP[S](f: (S, Action) => S): Effect[S] = new Effect[S]((_, _) => true, f)

  def apply[S](f: S => S): Effect[S] = new Effect((_, _) => true, { case (s, _) => f(s) })

  def apply[S](precondition: S => Boolean, f: S => S): Effect[S] = new Effect({ case (s, _) => precondition(s) }, { case (s, _) => f(s) })

  def precondition[S](precondition: S => Boolean): Effect[S] = new Effect({ case (s, _) => precondition(s) }, { case (s, _) => s })

}

// It will be hard to test effects if doesn't consider actions
case class Effect[S](precondition: (S, Action) => Boolean, f: (S, Action) => S) extends Function1[(S, Action), S] {
  def apply(input: (S, Action)): S = if (precondition(input._1, input._2)) f(input._1, input._2) else throw UnsatisfiedPreconditionException

}

case class ActionSignature(in: Set[Action], out: Set[Action], int: Set[Action]) {
  require(in intersect out intersect int isEmpty, "The sets in,out & int must be disjoint")

  def acts(): Set[Action] = in union out union int

  def ext(): Set[Action] = in union out

  def local(): Set[Action] = out union int

  def compatible(that: ActionSignature): Boolean = {
    // require that the output actions of composed automata be disjoint
    val _out = out intersect that.out
    // require that internal actions of each automaton in a composition be disjoint from the actions of the remaining automata
    val _in0 = int intersect that.acts
    val _in1 = that.int intersect acts
    (_out isEmpty) && (_in0 isEmpty) && (_in1 isEmpty)
  }

  def compose(that: ActionSignature): Option[ActionSignature] = if (compatible(that)) {
    val _in = (in union that.in) -- (out union that.out)
    val _out = (out union that.out)
    val _int = (int union that.int)
    Some(ActionSignature(in = _in, out = _out, int = _int))
  } else None

  def hide(actions: Set[Action]): ActionSignature = {
    val _in = in -- actions
    val _out = out -- actions
    val _int = int union (acts intersect actions)
    copy(in = _in, out = _out, int = _int)
  }
}


object Transition {
  type Transition[S] = PartialFunction[Action, Effect[S]]
}

object Steps {

  trait Steps[S] extends PartialFunction[(S, Action), S] {

    def isEnabled(state: S, d: Action): Boolean

  }

  def steps[S](transition: Transition[S]): Steps[S] = new Steps[S] {
    // 1. how to model input enabled? if I can't tell if input/output a property of the ActionSignature (not a action property itself)
    // 2. If a precondition doesn't hold (but the action matches), doesn't perform the action or fails (throw exception) ?
    // 3. Si la action no cumple le precondition no deberia modificar la execution! es decir no tomaria un step.
    //(v1: (S, Action)) => a.applyOrElse(v1, (v: (S,Action)) => v._1)

    override def isEnabled(state: S, d: Action): Boolean = isDefinedAt((state, d)) && transition.apply(d).precondition.apply(state, d) // TODO UGLY AS SHIT!

    override def isDefinedAt(x: (S, Action)): Boolean = transition.isDefinedAt(x._2)

    // TODO transition.apply puede dar un MatchInput error, usar un applyOrElse q tire exception
    override def apply(v1: (S, Action)): S = v1 match {
      case (state, action) if transition.isDefinedAt(action) => transition.apply(action).apply(state, action)
      //case _ => throw UnknownActionException TODO review
    }
  }

  def compose[S1, S2, S3](steps1: Steps[S1], steps2: Steps[S2], f: (S1, S2) => S3)(implicit f1: S3 => S1, f2: S3 => S2): Steps[S3] = new Steps[S3] {
    override def isEnabled(state: S3, d: Action): Boolean = {
      val (s1, s2) = (f1(state), f2(state))
      (isDefinedAt((state, d))) && (!steps1.isDefinedAt((s1, d)) || steps1.isEnabled(s1, d)) && (!steps2.isDefinedAt((s2, d)) || steps2.isEnabled(s2, d))
    }

    override def isDefinedAt(x: (S3, Action)): Boolean = x match {
      case (state, action) =>
        val (s1, s2) = (f1(state), f2(state))
        (steps1.isDefinedAt(s1, action)) || (steps2.isDefinedAt(s2, action))
    }

    override def apply(v1: (S3, Action)): S3 = {
      def aux[A]: ((A, Action)) => A = (s: (A, Action)) => s._1

      v1 match {
        case (state, action) if (isDefinedAt(v1)) =>
          val (s1, s2) = (f1(state), f2(state))
          val (n1, n2) = (steps1.applyOrElse((s1, action), aux), steps2.applyOrElse((s2, action), aux))
          f(n1, n2)
        case _ => throw UnknownActionException
      }
    }
  }
}

trait Automaton[S] {

  private final case class HideAutomaton[S](automaton: Automaton[S], actions: Set[Action]) extends Automaton[S] {
    override val sig: ActionSignature = automaton.sig.hide(actions)
    override val steps: Steps[S] = automaton.steps // TODO should impact in stepts to? (i think so)
  }

  private final case class CompositeAutomaton[S, S1, S2](aut1: Automaton[S1], aut2: Automaton[S2], f: (S1, S2) => S)(implicit f1: S => S1, f2: S => S2) extends Automaton[S] {
    override val sig: ActionSignature = aut1.sig.compose(aut2.sig).get // TODO remove get
    override val steps: Steps[S] = Steps.compose(aut1.steps, aut2.steps, f)(f1, f2)
  }

  val sig: ActionSignature

  val steps: Steps[S]

  def compose[S2, S3](that: Automaton[S2], f: (S, S2) => S3)(implicit f1: S3 => S, f2: S3 => S2): Option[Automaton[S3]] = {
    def compatible(that: Automaton[_]) = this.sig.compatible(that.sig)

    if (compatible(that)) Some(CompositeAutomaton(this, that, f))
    else None
  }

  def composeTuple[S2](that: Automaton[S2]): Option[Automaton[(S, S2)]] = compose(that, (s1: S, s2: S2) => (s1, s2))(_._1, _._2)


  def hide(actions: Set[Action]): Automaton[S] = HideAutomaton(this, actions)

}

object Composer {

  type STuple2[S] = Tuple2[S, S]
  type STuple3[S] = Tuple3[S, S, S]
  type STuple4[S] = Tuple4[S, S, S, S]
  type STuple5[S] = Tuple5[S, S, S, S, S]
  type STuple6[S] = Tuple6[S, S, S, S, S, S]
  type STuple7[S] = Tuple7[S, S, S, S, S, S, S]

  def composeTuple2[S2](a1: Automaton[(S2, S2)], a2: Automaton[S2]): Option[Automaton[(S2, S2, S2)]] = a1.compose(a2, (s1: (S2, S2), s2: S2) => (s1._1, s1._2, s2))(s => (s._1, s._2), _._3)

  def composeTuple3[S2](a1: Automaton[(S2, S2, S2)], a2: Automaton[S2]): Option[Automaton[(S2, S2, S2, S2)]] = a1.compose(a2, (s1: (S2, S2, S2), s2: S2) => (s1._1, s1._2, s1._3, s2))(s => (s._1, s._2, s._3), _._4)

  def composeTuple4[S2](a1: Automaton[(S2, S2, S2, S2)], a2: Automaton[S2]): Option[Automaton[(S2, S2, S2, S2, S2)]] = a1.compose(a2, (s1: (S2, S2, S2, S2), s2: S2) => (s1._1, s1._2, s1._3, s1._4, s2))(s => (s._1, s._2, s._3, s._4), _._5)

  def composeTuple5[S2](a1: Automaton[(S2, S2, S2, S2, S2)], a2: Automaton[S2]): Option[Automaton[(S2, S2, S2, S2, S2, S2)]] = a1.compose(a2, (s1: (S2, S2, S2, S2, S2), s2: S2) => (s1._1, s1._2, s1._3, s1._4, s1._5, s2))(s => (s._1, s._2, s._3, s._4, s._5), _._6)

}


case class Step[S](a0: S, d: Action, a1: S)

object Execution {

  //type Triggers = Map[Action,Set[Action]]
  type Triggers = PartialFunction[Action, Set[Action]]
  val EmptyTriggers: Triggers = PartialFunction.empty

}


case class Execution[S](automaton: Automaton[S], initialState: S, steps: List[Step[S]] = List.empty, triggers: Triggers = Execution.EmptyTriggers, round: Int = 0) {

  protected[Execution] def _next(state: S, d: Action): Execution[S] = {
    val nextState = automaton.steps.apply((state, d))
    val step = Step(state, d, nextState)
    copy(steps = steps :+ step)
  }

  def next(d: Action): Execution[S] = {
    val s = if (steps.isEmpty) _next(initialState, d) else _next(steps.last.a1, d)
    if (triggers.isDefinedAt(d)) triggers(d).foldLeft(s)(_ next _) // use PF orElse
    else s
  }

  // TODO esto no es propio de la teoria de IOAutomata
  def next(dos: Set[Action]): Execution[S] = {
    // el forall enabled deberia ser suficiente, sino podria ejecutar en todso los ordenes posibles y chequear q todos lleguen al mismo final state, sino throw exception
    if (dos.forall(isEnabled)) dos.foldLeft(this)(_ next _)
    else throw UnsatisfiedPreconditionException
  }

  def sched(): List[Action] = steps.map(_.d)

  // for debugging
  def state: S = if (steps.isEmpty) initialState else steps.last.a1

  def enabled(): Set[Action] = automaton.sig.acts().filter(act => automaton.steps.isEnabled(state, act))

  def isEnabled(d: Action): Boolean = automaton.steps.isEnabled(state, d)


}

object FLLProtocol {

  trait Envelope extends Action

  case class MessageID(from: ProcessID, to: ProcessID, payload: Any, step: Int)

  case class Message(payload: Any, id: MessageID)

  def send(from: ProcessID, to: ProcessID, payload: Any)(step: Int) = Send(from,to,Message(payload,MessageID(from,to,payload,step)))

  def deliver(from: ProcessID, to: ProcessID, payload: Any)(step: Int) = Deliver(from,to,Message(payload,MessageID(from,to,payload,step)))

  case class Send(from: ProcessID, to: ProcessID, message: Message) extends Envelope {
    override def toString: String = s"Send(from: $from, to: $to, payload: ${message.payload}, step: ${message.id.step})"
  }

  case class Deliver(from: ProcessID, to: ProcessID, message: Message) extends Envelope {
    override def toString: String = s"Deliver(from: $from, to: $to, payload: ${message.payload}, step: ${message.id.step})"
  }

}

object FairLossLinkStateN {
  def empty(): FairLossLinkStateN = new FairLossLinkStateN(Set.empty)

}

// TOOD should be something like Payload instead of Any (but Payloa dis Option[Any])
case class FairLossLinkStateN(messages: Set[Any]) {
  def add(message: Any): FairLossLinkStateN = copy(messages = messages + message)

  def remove(message: Any): FairLossLinkStateN = copy(messages = messages - message)

  def canDeliver(message: Any): Boolean = messages contains message
}

case class Payloads(payloads: Set[Any], steps: Int) {
  def sends(from: Int, to: Int): Set[Action] = (for (p <- payloads; s <- 0 until steps) yield (p,s)).map { case (p,s) => FLLProtocol.send(from,to,p)(s) }

  def delivers(from: Int, to: Int): Set[Action] = (for (p <- payloads; s <- 0 until steps) yield (p,s)).map { case (p,s) =>FLLProtocol.deliver(from,to,p)(s) }
}

case class FairLossLinkN(from: ProcessID, to: ProcessID)(implicit payloads: Payloads) extends Automaton[FairLossLinkStateN] {
  override val sig: ActionSignature = {
    val in: Set[Action] = payloads.sends(from,to)
    val out: Set[Action] = payloads.delivers(from,to)
    val int: Set[Action] = Set.empty[Action]
    ActionSignature(in = in, out = out, int = int)
  }
  override val steps: Steps.Steps[FairLossLinkStateN] = {
    val transitions: Transition[FairLossLinkStateN] = {
      case Send(`from`, `to`, payload) if payloads.payloads contains payload.payload => Effect(state => state.add(payload))
      case Deliver(`from`, `to`, payload) if payloads.payloads contains payload.payload => Effect(state => state.canDeliver(payload), state => state.remove(payload))
    }
    Steps.steps(transitions)
  }
}

object Probando extends App {
  implicit val payloads = Payloads(Set(1, 2, 3),10)

  val automaton = FairLossLinkN(0, 1)

  val state = FairLossLinkStateN(Set.empty)

  println(Execution(automaton, state).enabled())
}