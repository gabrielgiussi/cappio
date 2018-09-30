package oss.ggiussi.cappio

import oss.ggiussi.cappio.Level.Condition
import oss.ggiussi.cappio.ProcessProtocol.Crash
import oss.ggiussi.cappio.impl.links.FLLProtocol.{Deliver, Send}
import oss.ggiussi.cappio.impl.links.{FairLossLink2, FairLossLinkState2}
import oss.ggiussi.cappio.impl.links.Protocol.ProcessID


object Level {

  type Condition[S] = S => Boolean

  def apply[S](stateConditions: List[Condition[S]],schedConditions: List[Condition[List[Action]]], automaton: Automaton[S], initialState: S): Level[S] = new Level(stateConditions,schedConditions, Execution2(automaton, initialState))
}

object ProcessProtocol {

  case class Crash(id: ProcessID) extends Action

}

sealed trait ProcessState {

  def deliver(msg: Any): ProcessState

}

case class Up(x: Int) extends ProcessState {

  def deliver(msg: Any): ProcessState = copy(x = msg.toString.toInt) // UGHHH

}

case object Down extends ProcessState {
  override def deliver(msg: Any): ProcessState = Down
}

case class Process(id: ProcessID, neighbors: Set[ProcessID]) extends Automaton[ProcessState] {
  override val sig: ActionSignature = {
    val in: Set[Action] = {
      val delivers: Set[Action] = neighbors.map(Deliver(_, id))
      println(delivers)
      delivers + Crash(id)
    }
    val out: Set[Action] = Set.empty
    val int: Set[Action] = Set.empty
    ActionSignature(in = in, out = out, int = int)
  }
  override val steps: Steps.Steps[ProcessState] = Steps.steps {
    case Deliver(from, `id`) if neighbors contains from => Effect.inputEnabledP({ case (state, Some(message)) => state deliver message })
    case Crash(`id`) => Effect.inputEnabledP({ case _ => Down })
  }

}

sealed trait LevelResult[S] {
  def level: Level[S]
}

case class Success[S](level: Level[S]) extends LevelResult[S]

case class Pending[S](level: Level[S]) extends LevelResult[S]

// un level es una execution + conditions
case class Level[S](stateConditions: List[Condition[S]], schedConditions: List[Condition[List[Action]]], execution: Execution2[S]) {

  def next(d: Do): LevelResult[S] = {
    val ex = execution.next(d)
    val level = copy(execution = ex)
    if ((stateConditions.forall(_.apply(ex.state))) && (schedConditions.forall(_.apply(ex.sched())))) Success(level) else Pending(level)
  }

}

object Prueba extends App {

  type State = (((FairLossLinkState2, FairLossLinkState2), ProcessState), ProcessState)

  val automaton: Option[Automaton[State]] = for {
    c1 <- FairLossLink2(0, 1).composeTuple(FairLossLink2(1, 0))
    c2 <- c1 composeTuple Process(0, Set(1))
    c3 <- c2 composeTuple Process(1, Set(0))
  } yield c3

  val initalState: Option[State] = for {
    c1 <- Some((FairLossLinkState2(Set.empty), FairLossLinkState2(Set.empty)))
    c2 <- Some((c1, Up(0)))
    c3 <- Some((c2, Up(0)))
  } yield c3

  val conditions: List[Condition[State]] = List(
    (s: State) => s._2.asInstanceOf[Up].x == 1,
    (s: State) => s._1._2.asInstanceOf[Up].x == 1,
    (s: State) => s._1._1._1.messages.collect { case e: Deliver => e }.isEmpty,
    (s: State) => s._1._1._2.messages.collect { case e: Deliver => e }.isEmpty
  )


  val level1 = Level(conditions,List.empty, automaton.get, initalState.get)

  println(level1.next(Do(Send(0, 1), Some(1)))
    .level.next(Do(Deliver(0, 1), Some(1)))
    .level.next(Do(Send(1,0), Some(1)))
    .level.next(Do(Send(1,0), Some(1)))
    .level.next(Do(Deliver(1,0), Some(1)))
  )


}
