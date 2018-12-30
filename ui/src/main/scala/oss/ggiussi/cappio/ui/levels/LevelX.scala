package oss.ggiussi.cappio.ui.levels

import oss.ggiussi.cappio.core.Level.Condition
import oss.ggiussi.cappio.core.LinkProtocol.DeliverHeader
import oss.ggiussi.cappio.core._
import oss.ggiussi.cappio.impl.links.{FLLState, FairLossLink, FullFLLState}
import oss.ggiussi.cappio.impl.processes.{Process, ProcessState, Up}

object LevelX {//extends LevelT {
/*
  def level(): Level[_] = {
    implicit val payloads = Payloads(Set(1, 2), 10)

    import Composer._

    type State = (STuple6[FLLState], STuple3[ProcessState])

    val processes: Option[Automaton[STuple3[ProcessState]]] = for {
      c1 <- Process(0, Set(1, 2)) composeTuple Process(1, Set(0, 2)) // FIXME los neighbors los necesito para poder crear las input actions....
      c2 <- composeTuple2(c1, Process(2, Set(1, 0)))
    } yield c2

    val links: Option[Automaton[STuple6[FullFLLState]]] = for {
      c1 <- FairLossLink(0, 1) composeTuple FairLossLink(1, 0)
      c2 <- composeTuple2(c1, FairLossLink(1, 2))
      c3 <- composeTuple3(c2, FairLossLink(2, 1))
      c4 <- composeTuple4(c3, FairLossLink(0, 2))
      c5 <- composeTuple5(c4, FairLossLink(2, 0))
    } yield c5

    val automaton: Option[Automaton[State]] = for {
      l <- links
      p <- processes
      a <- l composeTuple p
    } yield a

    val initalState: State = (
      (FLLState.empty, FLLState.empty, FLLState.empty, FLLState.empty, FLLState.empty, FLLState.empty),
      (Up(0), Up(0), Up(0))
    )

    val conditions: List[StateCondition[State]] = List(
      StateCondition((s: State) => s._2 match {
        case (Up(1), Up(1), Up(1)) => true
        case _ => false
      }),
      StateCondition((s: State) => List(s._1._1, s._1._2, s._1._3, s._1._4).forall(_.messages.collect { case e: Deliver => e }.isEmpty))
    )

    val schedConditions: List[Condition[List[Action]]] = List(
      (sched: List[Action]) => sched.exists {
        //case Send(0,1,2) => true TODO
        case _ => false
      }
    )

    Level(conditions, schedConditions, automaton.get, initalState)
  }
*/
}
