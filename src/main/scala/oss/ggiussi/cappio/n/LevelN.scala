package oss.ggiussi.cappio.n

import oss.ggiussi.cappio.Action
import oss.ggiussi.cappio.Level.Condition
import oss.ggiussi.cappio.ProcessProtocol.Crash
import oss.ggiussi.cappio.impl.links.Protocol.{FLLDeliver, ProcessID}
import oss.ggiussi.cappio.n.FLLProtocol.{Deliver, Send}


object LevelN {

  type Condition[S] = S => Boolean

  def apply[S](conditions: List[Condition[S]], schedConditions: List[Condition[List[Action]]], automaton: Automaton[S], initialState: S): LevelN[S] = new LevelN(conditions, schedConditions, List(Execution2(automaton, initialState)))
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

case class ProcessN(id: ProcessID, neighbors: Set[ProcessID])(implicit payloads: Payloads) extends Automaton[ProcessState] {
  override val sig: ActionSignature = {
    val in: Set[Action] = {
      val delivers: Set[Action] = neighbors.map(payloads.delivers(_, id)).flatten //neighbors.flatMap(n => payloads.map(Deliver(n, id, _)))
      delivers + Crash(id)
    }
    val out: Set[Action] = Set.empty // FIXME send es una output action, porque esta en steps.
    val int: Set[Action] = Set.empty
    ActionSignature(in = in, out = out, int = int)
  }
  override val steps: Steps.Steps[ProcessState] = Steps.steps {
    case FLLProtocol.Send(`id`, _, payload) if payloads.payloads contains payload => Effect.precondition(_.isInstanceOf[Up])
    case FLLProtocol.Deliver(_, `id`, payload) if payloads.payloads contains payload => Effect(_.isInstanceOf[Up], _ deliver payload)
    case Crash(`id`) => Effect(_.isInstanceOf[Up], _ => Down)
  }

}

sealed trait LevelResult[S] {
  def level: LevelN[S]

  def ended: Boolean
}

case class Success[S](level: LevelN[S]) extends LevelResult[S] {
  override def ended: Boolean = true
}

case class Pending[S](level: LevelN[S]) extends LevelResult[S] {
  override def ended: Boolean = false
}

case class Failed[S](level: LevelN[S]) extends LevelResult[S] {
  override def ended: Boolean = true
}

// un level es una execution + conditions
case class LevelN[S](conditions: List[Condition[S]], schedConditions: List[Condition[List[Action]]], executions: List[Execution2[S]]) {

  protected def last(): Execution2[S] = executions.last

  def next(action: Action): LevelResult[S] = {
    val ex = last.next(action)
    val level = copy(executions = executions :+ ex)
    if ((conditions.forall(_.apply(ex.state))) && (schedConditions.forall(_.apply(ex.sched())))) Success(level) else if (level.last().enabled().isEmpty) Failed(level) else Pending(level)
  }

  // TODO why pending?
  def prev(): LevelN[S] = if (executions.size == 1) this else copy(executions = executions.reverse.tail.reverse)

  def nextStep(): Int = executions.size - 1
}

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


  val level1 = LevelN(conditions, List.empty, automaton.get, initalState.get)

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