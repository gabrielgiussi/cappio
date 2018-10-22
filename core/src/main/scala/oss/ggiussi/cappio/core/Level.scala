package oss.ggiussi.cappio.core

import oss.ggiussi.cappio.core.Execution.Triggers
import oss.ggiussi.cappio.core.Level.Condition


object Level {

  type Condition[S] = S => Boolean

  def apply[S](conditions: List[StateCondition[S]], schedConditions: List[Condition[List[Action]]], automaton: Automaton[S], initialState: S, triggers: Option[Triggers] = None): Level[S] = new Level(conditions, schedConditions, List(Execution(automaton, initialState,triggers = triggers.getOrElse(Execution.EmptyTriggers))))
}

object StateCondition {
  def apply[S](f: S => Boolean): StateCondition[S] = new StateCondition("No description", f)
}

case class StateCondition[S](description: String, f: S => Boolean) extends (S => Boolean) {
  override def apply(v1: S): Boolean = f(v1)
}

sealed trait LevelResult[S] {
  def level: Level[S]

  def ended: Boolean
}

case class Success[S](level: Level[S]) extends LevelResult[S] {
  override def ended: Boolean = true
}

case class Pending[S](level: Level[S]) extends LevelResult[S] {
  override def ended: Boolean = false
}

case class Failed[S](level: Level[S]) extends LevelResult[S] {
  override def ended: Boolean = true
}

// un level es una execution + conditions
case class Level[S](conditions: List[StateCondition[S]], schedConditions: List[Condition[List[Action]]], executions: List[Execution[S]]) {

  protected def last(): Execution[S] = executions.last

  def next(action: Action): LevelResult[S] = {
    val ex = last.next(action)
    val level = copy(executions = executions :+ ex)
    if ((conditions.forall(_.apply(ex.state))) && (schedConditions.forall(_.apply(ex.sched())))) Success(level) else if (level.last().enabled().isEmpty) Failed(level) else Pending(level)
  }

  // TODO why pending?
  def prev(): Level[S] = if (executions.size == 1) this else copy(executions = executions.reverse.tail.reverse)

  def nextStep(): Int = executions.size - 1

  def state() = executions.lastOption.map(_.state)
}
