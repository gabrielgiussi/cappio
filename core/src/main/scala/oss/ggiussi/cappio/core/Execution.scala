package oss.ggiussi.cappio.core

case class Step[S](a0: S, action: Action, a1: S)

case class Execution[S](automaton: Automaton[S], initialState: S, steps: List[Step[S]] = List.empty, enabled: Set[Action] = Set.empty,monitor: List[Action => Set[Action]] = List.empty, round: Int = 0) {

  protected[Execution] def _next(state: S, d: Action): Execution[S] = {
    val NextState(nextState,t) = automaton.steps.apply((state, d))
    val step = Step(state, d, nextState)
    val more = monitor.foldLeft(Set.empty[Action])(_ ++ _(d))
    val ex = copy(steps = steps :+ step, enabled = enabled ++ more)
    t.foldLeft(ex)(_ next _)
  }

  // TODO next could fail! (e.g. if the action doesn't exists or isn't enabled, something like Try[Execution] or NextExecution -> failed or success)
  def next(d: Action): Execution[S] = if (steps.isEmpty) _next(initialState, d) else _next(steps.last.a1, d)

  // TODO esto no es propio de la teoria de IOAutomata
  def next(dos: Set[Action]): Execution[S] = {
    // el forall enabled deberia ser suficiente, sino podria ejecutar en todso los ordenes posibles y chequear q todos lleguen al mismo final state, sino throw exception
    if (dos.forall(isEnabled)) dos.foldLeft(this)(_ next _)
    else throw UnsatisfiedPreconditionException
  }

  def sched(): List[Action] = steps.map(_.action)

  // for debugging
  def state: S = if (steps.isEmpty) initialState else steps.last.a1

  // TODO
  //def enabled(): Set[ActionHeader] = automaton.sig.acts().filter(act => automaton.steps.isEnabled(state, act))

  def isEnabled(d: Action): Boolean = automaton.steps.isEnabled(state, d)

  def enabledActions: Set[Action] = enabled.filter(isEnabled)


}