package oss.ggiussi.cappio.core

import oss.ggiussi.cappio.core.Level.Condition


object Level {

  type Condition[S] = S => Boolean

  def apply[S](conditions: List[StateCondition[S]], schedConditions: List[Condition[List[Action]]], automaton: Automaton[S], initialState: S): Level[S] = new Level(conditions, schedConditions, List(Execution(automaton, initialState)))
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
/*
object Prueba extends App {

  implicit val payloads = Payloads(Set(1, 2), 10)

  type State = (((FairLossLinkStateN, FairLossLinkStateN), ProcessState), ProcessState)

  val automaton: Option[Automaton[State]] = for {
    c1 <- FairLossLinkN(0, 1).composeTuple(FairLossLinkN(1, 0))
    c2 <- c1 composeTuple ProcessN(0, Set(1))
    c3 <- c2 composeTuple ProcessN(1, Set(0))
  } yield c3

  val initalState: Option[State] = for {
    c1 <- Some((FairLossLinkStateN(Set.empty), FairLossLinkStateN(Set.empty)))
    c2 <- Some((c1, Up(0)))
    c3 <- Some((c2, Up(0)))
  } yield c3

  val conditions: List[Condition[State]] = List(
    (s: State) => s._2.isInstanceOf[Up] && s._2.asInstanceOf[Up].x == 1,
    (s: State) => s._1._2.isInstanceOf[Up] && s._1._2.asInstanceOf[Up].x == 1,
    (s: State) => s._1._1._1.messages.collect { case e: Deliver => e }.isEmpty,
    (s: State) => s._1._1._2.messages.collect { case e: Deliver => e }.isEmpty
  )


  val level1 = Level(conditions, List.empty, automaton.get, initalState.get)

  /*
  println(level1.next(Send(0, 1, 1))
    .level.next(Deliver(0, 1, 1))
    .level.next(Crash(0))
    .level.next(Send(1, 0, 1))
    .level.next(Send(1, 0, 1))
    .level.next(Deliver(1, 0, 1))
  )
  */


}
*/