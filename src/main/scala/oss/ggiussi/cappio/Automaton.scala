package oss.ggiussi.cappio

import oss.ggiussi.cappio.Steps.Steps
import oss.ggiussi.cappio.Transition.Transition

trait Action // TODO empty trait?

final case class ActionSignature(in: Set[Action], out: Set[Action], int: Set[Action]) {
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
  type Transition[S] = PartialFunction[(S, Action), S]

  def inputTransition[S](enabled: Action => Boolean, effect: (S, Action) => S): Transition[S] = transition(enabled, _ => true, effect)

  // TODO precondition only for output || internal
  def transition[S](enabled: Action => Boolean, precondition: S => Boolean, effect: (S, Action) => S): Transition[S] = new Transition[S] {
    override def isDefinedAt(x: (S, Action)): Boolean = enabled(x._2) && precondition(x._1)

    override def apply(v1: (S, Action)): S = effect(v1._1,v1._2)
  }

}

object Steps {
  type Steps[S] = Function1[(S, Action), S]

  def steps[S](transitions: Set[Transition[S]]): Steps[S] = {
    // 1. how to model input enabled? if I can't tell if input/output a property of the ActionSignature (not a action property itself)
    // 2. If a precondition doesn't hold (but the action matches), doesn't perform the action or fails (throw exception) ?
    val a = transitions.toList match {
      case x :: xs => xs.foldLeft(x)(_ orElse _)
      case Nil => throw new RuntimeException("TODO") // TODO
    }
    (v1: (S, Action)) => a.applyOrElse(v1, (v: (S,Action)) => v._1)
  }

  def compose[S1, S2, S3](steps1: Steps[S1], steps2: Steps[S2], f: (S1, S2) => S3)(implicit f1: S3 => S1, f2: S3 => S2): Steps[S3] = new Steps[S3] {
    override def apply(v1: (S3, Action)): S3 = f(steps1(v1._1,v1._2), steps2(v1._1,v1._2))
  }
}

trait Automaton[S] {

  private final case class CompositeAutomaton[S,S1,S2](aut1: Automaton[S1], aut2: Automaton[S2], f: (S1, S2) => S)(implicit f1: S => S1, f2: S => S2) extends Automaton[S] {
    override val sig: ActionSignature = aut1.sig.compose(aut2.sig).get // TODO remove get
    override val steps: Steps[S] = Steps.compose(aut1.steps, aut2.steps, f)(f1,f2)
  }

  val sig: ActionSignature

  val steps: Steps[S]

  def compose[S2, S3](that: Automaton[S2], f: (S, S2) => S3)(implicit f1: S3 => S, f2: S3 => S2): Option[Automaton[S3]] = {
    def compatible(that: Automaton[_]) = this.sig.compatible(that.sig)
    if (compatible(that)) Some(CompositeAutomaton(this,that,f))
    else None
  }


  def hide(actions: Set[Action]): Automaton[S] = ??? // TODO

}

// actions: no deberia poder recibir el parametro
// deberia llevar la lista de estados
case class Execution[S](automaton: Automaton[S], state: S, actions: List[Action] = List()){

  def next(action: Action): Execution[S] = copy(state = automaton.steps.apply((state,action)), actions = actions :+ action)

  def sched(): List[Action] = actions
}

