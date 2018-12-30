package oss.ggiussi.cappio.ui.levels

import oss.ggiussi.cappio.Processes
import oss.ggiussi.cappio.core.Composer._
import oss.ggiussi.cappio.core.Level.Condition
import oss.ggiussi.cappio.core.LinkProtocol._
import oss.ggiussi.cappio.core._
import oss.ggiussi.cappio.impl.Instances
import oss.ggiussi.cappio.impl.bcast.{BrokenBcast, BrokenBcastState}
import oss.ggiussi.cappio.impl.links.{FLLState, FairLossLink, FullFLLState}
import oss.ggiussi.cappio.impl.processes.{ProcessBcast, ProcessState, Up}
import oss.ggiussi.cappio.ui.app.Conditions

object Level1 extends LevelT[((FullFLLState,FullFLLState,FullFLLState,FLLState,FLLState,FLLState), STuple3[ProcessState], STuple3[BrokenBcastState])] {

  type State = ((FullFLLState,FullFLLState,FullFLLState,FLLState,FLLState,FLLState), STuple3[ProcessState], STuple3[BrokenBcastState])
  import Conditions._

  val conditions = List(
    Conditions.allProcessesUp[State](_._2),
    StateCondition("Process 0 state should be x = 1", (s: State) => s._2._1 == Up(1)),
    StateCondition("Process 1 state should be x = 1", (s: State) => s._2._2 == Up(1)),
    StateCondition("Process 2 state should be x = 0", (s: State) => s._2._3 == Up(0)),
    StateCondition("There must not be remaining messages to deliver", (s: State) => List(s._1._1, s._1._2, s._1._3).forall(_.empty) && List(s._1._4, s._1._5, s._1._6).forall(_.empty))
  )

  val schedConditions: List[(String, Condition[List[Action]])] = List()

  val level = {

    import Composer._

    implicit val p = Processes(Set(0,1,2))

    val processes: Option[Automaton[STuple3[ProcessState]]] = for {
      c1 <- ProcessBcast(0) composeTuple ProcessBcast(1)
      c2 <- composeTuple2(c1, ProcessBcast(2))
    } yield c2

    val bcast: Option[Automaton[STuple3[BrokenBcastState]]] = {
      val _bcast = BrokenBcast(Instances.BCAST) _
      for {
        c1 <- _bcast(0, Set(1, 2)) composeTuple _bcast(1, Set(0, 2))
        c2 <- composeTuple2(c1, _bcast(2, Set(1, 0)))
      } yield c2
    }

    val links = {
      val fd = FairLossLink.fullDuplex(Instances.BCAST_LINK) _
      val fl = FairLossLink.apply(Instances.BCAST_LINK) _
      for {
        c1 <- fd(0, 1) composeTuple fd(0, 2)
        c2 <- composeTuple2(c1, fd(1, 2))
        c3 <- composeTuple3(c2, fl(0, 0))
        c4 <- composeTuple4(c3, fl(1, 1))
        c5 <- composeTuple5(c4, fl(2, 2))
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
      (FullFLLState.empty, FullFLLState.empty, FullFLLState.empty, FLLState.empty, FLLState.empty, FLLState.empty),
      (Up(0), Up(0), Up(0)),
      (BrokenBcastState.empty, BrokenBcastState.empty, BrokenBcastState.empty)
    )

    println(automaton.get.sig.acts().mkString("\n"))

    Level(conditions, schedConditions.map(_._2), automaton.get, initalState, List({
      case Send(SendHeader(from,to,instance),msg) => Set(Deliver(DeliverHeader(from,to,instance),msg)) ++ (if (from == to) Set() else Set(Drop(DropHeader(from,to,instance),msg.id)))
      case _ => Set.empty
    }))

  }

}
