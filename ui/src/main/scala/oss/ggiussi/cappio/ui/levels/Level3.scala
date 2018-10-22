package oss.ggiussi.cappio.ui.levels

import oss.ggiussi.cappio.core.Composer._
import oss.ggiussi.cappio.core.Level.Condition
import oss.ggiussi.cappio.core.LinkProtocol.{Deliver, Send}
import oss.ggiussi.cappio.core._
import oss.ggiussi.cappio.impl.bcast.BrokenBcastProtocol.{BrkBcast, BrkDeliver}
import oss.ggiussi.cappio.impl.bcast.{BrokenBcast, BrokenBcastState}
import oss.ggiussi.cappio.impl.links._
import oss.ggiussi.cappio.impl.processes.{Down, ProcessBcast, ProcessState, Up}

object Level3 extends LevelT[((FullPLState,FullPLState,FullPLState,PLState,PLState,PLState), STuple3[ProcessState], STuple3[BrokenBcastState])] {

  type State = ((FullPLState,FullPLState,FullPLState,PLState,PLState,PLState), STuple3[ProcessState], STuple3[BrokenBcastState])

  val conditions = List(
    StateCondition("Process 0 state should be x = 1", (s: State) => s._2._1 == Up(1)),
    StateCondition("Process 1 state should be x = 1", (s: State) => s._2._2 == Up(1)),
    StateCondition("Process 2 state should be x = 0", (s: State) => s._2._3 != Up(1)),
    StateCondition("There must not be remaining messages to deliver", (s: State) => {
      val a = List(s._1._4, s._1._5).forall(_.isEmpty) && List(s._1._1, s._1._2).forall(_.isEmpty)
      val b = (s._1._6.isEmpty() && s._1._3.isEmpty()) || s._2._3 == Down
      a && b
    }),
  )

  val schedConditions: List[(String, Condition[List[Action]])] = List()

  val level = {
    implicit val payloads = Payloads(Set(1, 2), 10)

    import Composer._

    val processes: Option[Automaton[STuple3[ProcessState]]] = for {
      c1 <- ProcessBcast(0, Set(1, 2)) composeTuple ProcessBcast(1, Set(0, 2))
      c2 <- composeTuple2(c1, ProcessBcast(2, Set(1, 0)))
    } yield c2

    val bcast: Option[Automaton[STuple3[BrokenBcastState]]] = for {
      c1 <- BrokenBcast(0, Set(1, 2)) composeTuple BrokenBcast(1, Set(0, 2))
      c2 <- composeTuple2(c1, BrokenBcast(2, Set(1, 0)))
    } yield c2

    val links = for {
      c1 <- PerfectLink.fullDuplex(0, 1) composeTuple PerfectLink.fullDuplex(0, 2)
      c2 <- composeTuple2(c1, PerfectLink.fullDuplex(1, 2))
      c3 <- composeTuple3(c2, PerfectLink(0, 0))
      c4 <- composeTuple4(c3, PerfectLink(1, 1))
      c5 <- composeTuple5(c4, PerfectLink(2, 2))
    } yield c5


    val automaton: Option[Automaton[State]] = for {
      l <- links
      p <- processes
      b <- bcast
      a <- l composeTuple p
      a2 <- composeTuple2(a, b)
    } yield a2

    val initalState: State = (
      (FullPLState.empty, FullPLState.empty, FullPLState.empty, PLState.empty, PLState.empty, PLState.empty),
      (Up(0), Up(0), Up(0)),
      (BrokenBcastState.empty, BrokenBcastState.empty, BrokenBcastState.empty)
    )

    Level(conditions, schedConditions.map(_._2), automaton.get, initalState, Some({
      case BrkBcast(from,p,step) =>
        val r: Set[Action] = Set(0,1,2).map(to => Send(from,to,Message(from,to,p,step)))
        println(r)
        r
      case Deliver(from,to,msg) => Set(BrkDeliver(from,to,msg))
      case _ => Set.empty
    }))
  }

}