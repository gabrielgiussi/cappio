package oss.ggiussi.cappio.impl.links

import oss.ggiussi.cappio.impl.links.Protocol.{Envelope, FLLDeliver, FLLSend, ProcessID}
import oss.ggiussi.cappio.impl.links.Protocol3.{PLDeliver, PLSend}
import oss.ggiussi.cappio._
import oss.ggiussi.cappio.impl.links.Protocol2.{SLDeliver, SLSend}


object Protocol3 {

  case class PLSend(from: ProcessID, to: ProcessID) extends Envelope

  case class PLDeliver(from: ProcessID, to: ProcessID) extends Envelope

}

object PerfectLink {

  def initial(from: ProcessID, to: ProcessID): (PerfectLink, PerfectLinkState) = (PerfectLink(from, to), PerfectLinkState(Set(), Set(), Set()))

}

case class PerfectLinkState(sent: Set[Any], sldelivered: Set[Any], pldelivered: Set[Any]) {
  def send(message: Any) = copy(sent = sent + message)

  def canslSend(message: Any): Boolean = sent contains message

  def slSent(message: Any) = if (canslSend(message)) copy(sent = sent - message) else throw UnsatisfiedPreconditionException

  def canDeliver(message: Any): Boolean = (sldelivered contains message) && !(pldelivered contains message)

  def deliver(message: Any) = if (canDeliver(message)) copy(pldelivered = pldelivered + message) else throw UnsatisfiedPreconditionException

  def slDelivered(message: Any) = copy(sldelivered = sldelivered + message)

}

case class PerfectLink(from: ProcessID, to: ProcessID) extends Automaton[PerfectLinkState] {

  override val sig: ActionSignature = {
    import Protocol3._
    val in: Set[Action] = Set(PLSend(from, to), SLDeliver(from, to))
    val out: Set[Action] = Set(PLDeliver(from, to), SLSend(from, to))
    val int: Set[Action] = Set()
    ActionSignature(in = in, out = out, int = int)
  }

  override val steps: Steps.Steps[PerfectLinkState] = Steps.steps({
    case PLSend(`from`, `to`) => Effect.inputEnabledP({ case (state, Some(message)) => state send message })
    case PLDeliver(`from`, `to`) => new Effect({ case (state, Some(message)) => state canDeliver message }, { case (state, Some(message)) => state deliver message })
    case SLSend(`from`, `to`) => new Effect({ case (state, Some(message)) => state canslSend message }, { case (state, Some(message)) => state slSent message })
    case SLDeliver(`from`, `to`) => Effect.inputEnabledP({ case (state, Some(message)) => state slDelivered message })
  })
}

object PerfectLinkPrueba extends App {

  val link01 = FairLossLink(0, 1)
  val initialState01 = FairLossLinkState(Set.empty)

  val link10 = FairLossLink(1, 0)
  val initialState10 = FairLossLinkState(Set.empty)


  val stubborn01 = StubbornLink(0, 1)
  val initalStateSL01 = StubbornLinkState(Set.empty, Set.empty)

  val stubborn10 = StubbornLink(1, 0)
  val initalStateSL10 = StubbornLinkState(Set.empty, Set.empty)

  val (perfectLink01, plState01) = PerfectLink.initial(0, 1)
  val (perfectLink10, plState10) = PerfectLink.initial(1, 0)

  val automaton = for {
    c1 <- link01.composeTuple(link10)
    c2 <- c1.composeTuple(stubborn01)
    c3 <- c2.composeTuple(stubborn10)
    c4 <- c3.composeTuple(perfectLink01)
    c5 <- c4.composeTuple(perfectLink10)
  } yield (c5)

  val state = (((((initialState01, initialState10), initalStateSL01), initalStateSL10), plState01), plState10)
  /*
    println(
      Execution.execute(automaton.get, state, Do(PLSend(0, 1), Some(1)))
        .next(Do(SLSend(0, 1), Some(1)))
        .next(Do(FLLSend(0, 1), Some(1)))
        .next(Do(FLLDeliver(0, 1), Some(1)))
        .next(Do(SLDeliver(0, 1), Some(1)))
        .next(Do(SLDeliver(0, 1), Some(1)))
        .next(Do(PLDeliver(0, 1), Some(1)))
    .state)

    println(Execution.execute(automaton.get, state, Do(PLSend(0, 1), Some(1)))
      .next(Set(
        Do(PLSend(0,1),Some(2)),
        Do(PLSend(1,0),Some(3))
      )).state)
  */
  val e = Execution.executeM(automaton.get, state, Set(Do(PLSend(0, 1), Some(1)), Do(PLSend(1, 0), Some(2))), {
    case Do(PLSend(from, to), payload) => Set(Do(SLSend(from, to), payload), Do(FLLSend(from, to), payload)) // FIXME el trigger de PL solo deberia saber sobre SL, no FLL (necesito composicion de triggers)
    case Do(FLLDeliver(from, to), payload) => Set(Do(SLDeliver(from, to), payload), Do(PLDeliver(from, to), payload))
  })
  //println(e.next(Set(Do(PLDeliver(1, 0), Some(2)), Do(PLDeliver(0,1), Some(1)))).sched())

}