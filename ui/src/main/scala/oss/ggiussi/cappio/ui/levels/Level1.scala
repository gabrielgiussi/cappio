package oss.ggiussi.cappio.ui.levels

import oss.ggiussi.cappio.core.Composer._
import oss.ggiussi.cappio.core.Level.Condition
import oss.ggiussi.cappio.core._
import oss.ggiussi.cappio.impl.Instances
import oss.ggiussi.cappio.impl.bcast.{BrokenBcast, BrokenBcastState}
import oss.ggiussi.cappio.impl.links.{FLLState, FairLossLink, FullFLLState}
import oss.ggiussi.cappio.impl.processes.{ProcessBcast, ProcessState, Up}
import oss.ggiussi.cappio.ui.app.Conditions

object Level1 extends LevelT[(STuple6[FullFLLState], STuple3[ProcessState], STuple3[BrokenBcastState])] {

  type State = (STuple6[FullFLLState], STuple3[ProcessState], STuple3[BrokenBcastState])
  import Conditions._

  val conditions = List(
    Conditions.allProcessesUp[State](_._2),
    StateCondition("Process 0 state should be x = 1", (s: State) => s._2._1 == Up(1)),
    StateCondition("Process 1 state should be x = 1", (s: State) => s._2._2 == Up(1)),
    StateCondition("Process 2 state should be x = 0", (s: State) => s._2._3 == Up(0)),
    StateCondition("There must not be remaining messages to deliver", (s: State) => List(s._1._1, s._1._2, s._1._3, s._1._4, s._1._5, s._1._6).forall(_.empty))
  )

  val schedConditions: List[(String, Condition[List[Action]])] = List()

  val level = {
    implicit val payloads = Payloads(Set(1, 2), 10)

    import Composer._

    val processes: Option[Automaton[STuple3[ProcessState]]] = for {
      c1 <- ProcessBcast(0, Set(1, 2)) composeTuple ProcessBcast(1, Set(0, 2))
      c2 <- composeTuple2(c1, ProcessBcast(2, Set(1, 0)))
    } yield c2

    val bcast: Option[Automaton[STuple3[BrokenBcastState]]] = {
      val _bcast = BrokenBcast(Instances.BCAST) _
      for {
        c1 <- _bcast(0, Set(1, 2)) composeTuple _bcast(1, Set(0, 2))
        c2 <- composeTuple2(c1, _bcast(2, Set(1, 0)))
      } yield c2
    }

    val links = {
      val fll = FairLossLink(Instances.BCAST_LINK) _
      for {
        c1 <- fll(0, 1) composeTuple fll(0, 2)
        c2 <- composeTuple2(c1, fll(1, 2))
        c3 <- composeTuple3(c2, fll(0, 0))
        c4 <- composeTuple4(c3, fll(1, 1))
        c5 <- composeTuple5(c4, fll(2, 2))
      } yield c5
    }


    val automaton: Option[Automaton[State]] = for {
      l <- links
      p <- processes
      b <- bcast
      a <- l composeTuple p
      a2 <- composeTuple2(a, b)
    } yield a2

    val initalState: State = (
      (FullFLLState.empty, FullFLLState.empty, FullFLLState.empty, FullFLLState.empty, FullFLLState.empty, FullFLLState.empty),
      (Up(0), Up(0), Up(0)),
      (BrokenBcastState.empty, BrokenBcastState.empty, BrokenBcastState.empty)
    )

    Level(conditions, schedConditions.map(_._2), automaton.get, initalState)
  }

}
