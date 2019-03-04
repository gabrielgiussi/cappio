package oss.ggiussi.cappio.core

import oss.ggiussi.cappio.InstanceID
import oss.ggiussi.cappio.core.Steps.Steps
import oss.ggiussi.cappio.core.Transition.{Transition, Transition2}

// TODO rename
case object UnsatisfiedPreconditionException extends RuntimeException

case class UnknownActionException(a: Action) extends RuntimeException {
  override def getMessage: String = s"Action $a unknown"
}

trait ActionHeader {

  val instance: InstanceID

}

/*
object ActionBody {
  def empty = ActionBody(None,None)

  def apply(payload: Any): ActionBody = new ActionBody(Some(payload), None)
}

case class ActionBody(payload: Option[Any], metadata: Option[Any])

object Action {
  def apply(header: ActionHeader): Action = new Action(header, ActionBody.empty)
}
*/
trait Action {
  def header: ActionHeader
  def payload: Option[Any]
  def metadata: Option[Any] = None
}

trait NoPayloadAction extends Action {
  final override def payload: Option[Any] = None
}

trait SimpleAction extends ActionHeader with NoPayloadAction {
  override def header: ActionHeader = this
}

case class NextState[S](state: S, triggers: Set[Action] = Set.empty)

object Effect {

  // // or Try[S] TODO
  // enrealidad precondition y f dependen del payload del action tmb, pero lo estoy capturando en un closure (es menos prolijo pero me deja escribir menos verbose)
  //def inputEnabled[S](f: S => S): Effect[S] = new Effect((_, _) => true, { case (s, _) => f(s) })

  //def inputEnabledP[S](f: (S, Action) => S): Effect[S] = new Effect[S]((_, _) => true, f)

  def apply[S](f: S => S): Effect[S] = new Effect((_, _) => true, { case (s, _) => NextState(f(s)) })

  def triggers[S](f: S => NextState[S]): Effect[S] = new Effect((_, _) => true, { case (s, _) => f(s) })

  def triggers[S](precondition: S => Boolean, f: S => NextState[S]): Effect[S] = new Effect({ case (s, _) => precondition(s) }, { case (s, _) => f(s) })

  def apply[S](precondition: S => Boolean, f: S => S): Effect[S] = new Effect({ case (s, _) => precondition(s) }, { case (s, _) => NextState(f(s)) })

  def precondition[S](precondition: S => Boolean): Effect[S] = new Effect({ case (s, _) => precondition(s) }, { case (s, _) => NextState(s) })

  def noEffect[S](): Effect[S] = new Effect((_, _) => true, { case (s, _) => NextState(s) })

  def noEffect[S](precondition: S => Boolean): Effect[S] = new Effect((s,_) => precondition(s), { case (s, _) => NextState(s) })

}

// It will be hard to test effects if doesn't consider actions
case class Effect[S](precondition: (S, Action) => Boolean, f: (S, Action) => NextState[S]) extends Function1[(S, Action), NextState[S]] {
  def apply(input: (S, Action)) = {
    println(input)
    if (precondition(input._1, input._2)) f(input._1, input._2) else throw UnsatisfiedPreconditionException
  }

}

case class ActionSignature(in: Set[ActionHeader], out: Set[ActionHeader], int: Set[ActionHeader]) {
  require(in intersect out intersect int isEmpty, "The sets in,out & int must be disjoint")

  def acts(): Set[ActionHeader] = in union out union int

  def ext(): Set[ActionHeader] = in union out

  def local(): Set[ActionHeader] = out union int

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

  def hide(actions: Set[ActionHeader]): ActionSignature = {
    val _in = in -- actions
    val _out = out -- actions
    val _int = int union (acts intersect actions)
    copy(in = _in, out = _out, int = _int)
  }

  def contains(action: ActionHeader): Boolean = (in contains action) || (out contains action) || (int contains action)


}


object Transition {
  type Transition[S] = PartialFunction[Action, Effect[S]]

  type Transition2[S] = Function1[Action,Effect[S]]
}

object Steps {

  trait Steps[S] extends PartialFunction[(S, Action), NextState[S]] {

    def isEnabled(state: S, d: Action): Boolean

  }

  abstract class BaseSteps[S] extends Steps[S] {
    final override def isEnabled(state: S, d: Action): Boolean = isDefinedAt((state, d)) && effect(d).precondition.apply(state, d)

    final override def isDefinedAt(x: (S, Action)): Boolean = isDefined(x._2)

    final override def apply(v1: (S, Action)): NextState[S] = v1 match {
      case (state, action) if isDefined(action) => effect(action).apply(state, action)
      case (_, action) => throw new UnknownActionException(action) // TODO review
    }

    def isDefined(action: Action): Boolean

    def effect(action: Action): Effect[S]

  }

  // TODO rename
  def steps2[S](signature: ActionSignature,transition: Transition2[S]): Steps[S] = new BaseSteps[S] {

    override def isDefined(action: Action): Boolean = signature contains action.header

    override def effect(action: Action): Effect[S] = transition(action)
  }

  def steps[S](transition: Transition[S]): Steps[S] = new BaseSteps[S] {
    override def isDefined(action: Action): Boolean = transition.isDefinedAt(action)

    override def effect(action: Action): Effect[S] = transition(action)
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

    override def apply(v1: (S3, Action)): NextState[S3] = {
      def aux[A]: ((A, Action)) => NextState[A] = (s: (A, Action)) => NextState(s._1)

      v1 match {
        case (state, action) if (isDefinedAt(v1)) =>
          val (s1, s2) = (f1(state), f2(state))
          val (n1, n2) = (steps1.applyOrElse((s1, action), aux), steps2.applyOrElse((s2, action), aux))
          NextState(f(n1.state, n2.state), n1.triggers ++ n2.triggers)
        case (_, action) => throw new UnknownActionException(action)
      }
    }
  }
}

trait Automaton[S] {

  private final case class HideAutomaton[S](automaton: Automaton[S], actions: Set[ActionHeader]) extends Automaton[S] {
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

  def hide(actions: Set[ActionHeader]): Automaton[S] = HideAutomaton(this, actions)

  def inputAction(action: ActionHeader): Boolean = sig.in contains action

  def outputAction(action: ActionHeader): Boolean = sig.out contains action

  def internalAction(action: ActionHeader): Boolean = sig.int contains action

}

object Composer {

  type STuple2[S] = Tuple2[S, S]
  type STuple3[S] = Tuple3[S, S, S]
  type STuple4[S] = Tuple4[S, S, S, S]
  type STuple5[S] = Tuple5[S, S, S, S, S]
  type STuple6[S] = Tuple6[S, S, S, S, S, S]
  type STuple7[S] = Tuple7[S, S, S, S, S, S, S]

  def composeTuple2[S1, S2, S3](a1: Automaton[(S1, S2)], a2: Automaton[S3]): Option[Automaton[(S1, S2, S3)]] = a1.compose(a2, (s1: (S1, S2), s2: S3) => (s1._1, s1._2, s2))(s => (s._1, s._2), _._3)

  def composeTuple3[S1, S2, S3, S4](a1: Automaton[(S1, S2, S3)], a2: Automaton[S4]): Option[Automaton[(S1, S2, S3, S4)]] = a1.compose(a2, (s1: (S1, S2, S3), s2: S4) => (s1._1, s1._2, s1._3, s2))(s => (s._1, s._2, s._3), _._4)

  def composeTuple4[S1, S2, S3, S4, S5](a1: Automaton[(S1, S2, S3, S4)], a2: Automaton[S5]): Option[Automaton[(S1, S2, S3, S4, S5)]] = a1.compose(a2, (s1: (S1, S2, S3, S4), s2: S5) => (s1._1, s1._2, s1._3, s1._4, s2))(s => (s._1, s._2, s._3, s._4), _._5)

  def composeTuple5[S1, S2, S3, S4, S5, S6](a1: Automaton[(S1, S2, S3, S4, S5)], a2: Automaton[S6]): Option[Automaton[(S1, S2, S3, S4, S5, S6)]] = a1.compose(a2, (s1: (S1, S2, S3, S4, S5), s2: S6) => (s1._1, s1._2, s1._3, s1._4, s1._5, s2))(s => (s._1, s._2, s._3, s._4, s._5), _._6)

}
