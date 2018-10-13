package oss.ggiussi.cappio.core

import oss.ggiussi.cappio.core.Execution.Triggers

case class Step[S](a0: S, action: Action, a1: S)

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

  def sched(): List[Action] = steps.map(_.action)

  // for debugging
  def state: S = if (steps.isEmpty) initialState else steps.last.a1

  def enabled(): Set[Action] = automaton.sig.acts().filter(act => automaton.steps.isEnabled(state, act))

  def isEnabled(d: Action): Boolean = automaton.steps.isEnabled(state, d)


}
