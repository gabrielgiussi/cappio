package oss.ggiussi.cappio

import oss.ggiussi.cappio.Do.Payload
import oss.ggiussi.cappio.Execution.Triggers
import oss.ggiussi.cappio.Steps.Steps
import oss.ggiussi.cappio.Transition.Transition

object Do {
  type Payload = Option[Any]

  def apply(act: Action): Do = new Do(act, None)
}

// TODO rename
case class Do(action: Action, payload: Payload)

trait Action // TODO empty trait?

case object UnsatisfiedPreconditionException extends RuntimeException

case object UnknownActionException extends RuntimeException

object Effect {
  // // or Try[S] TODO
  // enrealidad precondition y f dependen del payload del action tmb, pero lo estoy capturando en un closure (es menos prolijo pero me deja escribir menos verbose)
  def inputEnabled[S](f: S => S): Effect[S] = new Effect((_, _) => true, { case (s, _) => f(s) })

  def inputEnabledP[S](f: (S, Payload) => S): Effect[S] = new Effect[S]((_, _) => true, f)

  def apply[S](precondition: S => Boolean, f: S => S): Effect[S] = new Effect({ case (s, _) => precondition(s) }, { case (s, _) => f(s) })

}

// It will be hard to test effects if doesn't consider actions
case class Effect[S](precondition: (S, Payload) => Boolean, f: (S, Payload) => S) extends Function1[(S, Payload), S] {
  def apply(input: (S, Payload)): S = if (precondition(input._1, input._2)) f(input._1, input._2) else throw UnsatisfiedPreconditionException

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

  trait Steps[S] extends PartialFunction[(S, Do), S] {

    def isEnabled(state: S, d: Do): Boolean

  }

  def steps[S](transition: Transition[S]): Steps[S] = new Steps[S] {
    // 1. how to model input enabled? if I can't tell if input/output a property of the ActionSignature (not a action property itself)
    // 2. If a precondition doesn't hold (but the action matches), doesn't perform the action or fails (throw exception) ?
    // 3. Si la action no cumple le precondition no deberia modificar la execution! es decir no tomaria un step.
    //(v1: (S, Action)) => a.applyOrElse(v1, (v: (S,Action)) => v._1)

    override def isEnabled(state: S, d: Do): Boolean = isDefinedAt((state, d)) && transition.apply(d.action).precondition.apply(state, d.payload) // TODO UGLY AS SHIT!

    override def isDefinedAt(x: (S, Do)): Boolean = transition.isDefinedAt(x._2.action)

    // TODO transition.apply puede dar un MatchInput error, usar un applyOrElse q tire exception
    override def apply(v1: (S, Do)): S = v1 match {
      case (state, Do(action, payload)) if transition.isDefinedAt(action) => transition.apply(action).apply(state, payload)
      //case _ => throw UnknownActionException TODO review
    }
  }

  def compose[S1, S2, S3](steps1: Steps[S1], steps2: Steps[S2], f: (S1, S2) => S3)(implicit f1: S3 => S1, f2: S3 => S2): Steps[S3] = new Steps[S3] {
    override def isEnabled(state: S3, d: Do): Boolean = {
      val (s1, s2) = (f1(state), f2(state))
      (isDefinedAt((state, d))) && (!steps1.isDefinedAt((s1, d)) || steps1.isEnabled(s1, d)) && (!steps2.isDefinedAt((s2, d)) || steps2.isEnabled(s2, d))
    }

    override def isDefinedAt(x: (S3, Do)): Boolean = x match {
      case (state, action) =>
        val (s1, s2) = (f1(state), f2(state))
        (steps1.isDefinedAt(s1, action)) || (steps2.isDefinedAt(s2, action))
    }

    override def apply(v1: (S3, Do)): S3 = {
      def aux[A]: ((A, Do)) => A = (s: (A, Do)) => s._1

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


case class Step[S](a0: S, d: Do, a1: S)

object Execution {

  //type Triggers = Map[Action,Set[Action]]
  type Triggers = PartialFunction[Do,Set[Do]]
  val EmptyTriggers: Triggers = PartialFunction.empty

  def execute[S](automaton: Automaton[S], initialState: S, d: Do, triggers: Triggers = Execution.EmptyTriggers, dependencies: Triggers = Execution.EmptyTriggers): Execution[S] = {
    //val a1 = automaton.steps.apply((initialState, d))
    //new Execution(automaton, List(Step(initialState, d, a1)), triggers)
    //new Execution[S](automaton, List.empty, triggers).inital(initialState,d)
    executeM(automaton,initialState,Set(d),triggers,dependencies)
  }

  def executeM[S](automaton: Automaton[S], initialState: S, ds: Set[Do], triggers: Triggers = Execution.EmptyTriggers, dependencies: Triggers = Execution.EmptyTriggers): Execution[S] = {
    //val a1 = automaton.steps.apply((initialState, d))
    //new Execution(automaton, List(Step(initialState, d, a1)), triggers)
    ds.tail.foldLeft(new Execution[S](automaton, List.empty, triggers,dependencies).inital(initialState,ds.head))(_ next _)

  }

  //def initiallyEnabled[S](automaton: Automaton[S], initialState: S): Set[Action] = automaton.sig.acts().filter(act => automaton.steps.isEnabled(initialState,act))

}

case class Execution[S](automaton: Automaton[S], steps: List[Step[S]], triggers: Triggers, dependencies: Triggers, round: Int = 0) {

  protected[Execution] def _next(state: S, d: Do): Execution[S] = {
    val nextState = automaton.steps.apply((state, d))
    val step = Step(state, d, nextState)
    copy(steps = steps :+ step)
  }

  protected[Execution] def inital(state: S, d: Do): Execution[S] = {
    val s = _next(state,d)
    if (triggers.isDefinedAt(d)) triggers(d).foldLeft(s)(_ next _) // use PF orElse
    else s
  }

  def next(d: Do): Execution[S] = {
    val s = _next(steps.last.a1,d)
    if (triggers.isDefinedAt(d)) triggers(d).foldLeft(s)(_ next _) // use PF orElse
    else s
  }

  // TODO esto no es propio de la teoria de IOAutomata
  def next(dos: Set[Do]): Execution[S] = {
    // el forall enabled deberia ser suficiente, sino podria ejecutar en todso los ordenes posibles y chequear q todos lleguen al mismo final state, sino throw exception
    if (dos.forall(isEnabled)) dos.foldLeft(this)(_ next _)
    else throw UnsatisfiedPreconditionException
  }

  def sched(): List[Action] = steps.map(_.d.action)

  def schedWithPayload(): List[Do] = steps.map(_.d)

  // for debugging
  def state: S = steps.last.a1

  //def enabled(): Set[Action] = automaton.sig.acts().filter(act => automaton.steps.isEnabled(state(),act))

  def isEnabled(d: Do): Boolean = automaton.steps.isEnabled(state, d)


}


case class Execution2[S](automaton: Automaton[S], initialState: S, steps: List[Step[S]] = List.empty, triggers: Triggers = Execution.EmptyTriggers, round: Int = 0) {

  protected[Execution2] def _next(state: S, d: Do): Execution2[S] = {
    val nextState = automaton.steps.apply((state, d))
    val step = Step(state, d, nextState)
    copy(steps = steps :+ step)
  }

  def next(d: Do): Execution2[S] = {
    val s = if (steps.isEmpty) _next(initialState,d) else _next(steps.last.a1,d)
    if (triggers.isDefinedAt(d)) triggers(d).foldLeft(s)(_ next _) // use PF orElse
    else s
  }

  // TODO esto no es propio de la teoria de IOAutomata
  def next(dos: Set[Do]): Execution2[S] = {
    // el forall enabled deberia ser suficiente, sino podria ejecutar en todso los ordenes posibles y chequear q todos lleguen al mismo final state, sino throw exception
    if (dos.forall(isEnabled)) dos.foldLeft(this)(_ next _)
    else throw UnsatisfiedPreconditionException
  }

  def sched(): List[Action] = steps.map(_.d.action)

  def schedWithPayload(): List[Do] = steps.map(_.d)

  // for debugging
  def state: S = if (steps.isEmpty) initialState else steps.last.a1

  //def enabled(): Set[Action] = automaton.sig.acts().filter(act => automaton.steps.isEnabled(state(),act))

  def isEnabled(d: Do): Boolean = automaton.steps.isEnabled(state, d)


}