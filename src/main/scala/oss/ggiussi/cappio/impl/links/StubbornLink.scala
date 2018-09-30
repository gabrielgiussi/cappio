package oss.ggiussi.cappio.impl.links

import oss.ggiussi.cappio.Transition.Transition
import oss.ggiussi.cappio._
import oss.ggiussi.cappio.impl.links.Protocol.{Envelope, FLLDeliver, FLLSend, Message, ProcessID}
import oss.ggiussi.cappio.impl.links.Protocol2.{SLDeliver, SLSend}

// TODO replace by Set[Message]
case class StubbornLinkState(sent: Set[Any], delivered: Set[Any]) {
  def send(message: Any) = copy(sent = sent + message)

  def canflSend(message: Any): Boolean = sent contains message

  def canDeliver(message: Any): Boolean = delivered contains message

  def deliver(message: Any) = if (canDeliver(message)) copy(delivered = delivered - message) else throw UnsatisfiedPreconditionException

  def flDelivered(message: Any) = copy(delivered = delivered + message)
}

object Protocol2 {

  case class SLSend(from: ProcessID, to: ProcessID) extends Envelope

  case class SLDeliver(from: ProcessID, to: ProcessID) extends Envelope

}

// TODO como represento el uses ????????? porque el stubbornlink siempre va  con un fairlosslink
case class StubbornLink(from: ProcessID, to: ProcessID) extends Automaton[StubbornLinkState] {
  override val sig: ActionSignature = {
    val in: Set[Action] = Set(SLSend(from, to), FLLDeliver(from, to))
    val out: Set[Action] = Set(SLDeliver(from, to), FLLSend(from, to))
    val int: Set[Action] = Set.empty[Action] // TODO tiene un timeout interno para reenviar!
    ActionSignature(in = in, out = out, int = int)
  }
  override val steps: Steps.Steps[StubbornLinkState] = {
    val transition: Transition[StubbornLinkState] = {
      case SLSend(`from`, `to`) => Effect.inputEnabledP({ case (state, payload) => state.send(payload.get) }) // TODO remove get
      case SLDeliver(`from`, `to`) => new Effect({ case (state, payload) => state.canDeliver(payload.get) }, { case (state, _) => state }) // TODO (importante) el estado podria modificarse si considero la internal action timeout! (no va a estar siempre disponible, si no cada cierto timeout)
      // Si voy a tener el automaton FLL hace falta tener estas actions aca? siento que estoy repitiendo logica del FLL, como chequear que el mensaje se haya enviado!
      // las preconditions y los effects de las actions (slsend - fllsend, sldeliver - flldeliver) son casi identicos
      case FLLDeliver(`from`, `to`) => Effect.inputEnabledP({ case (state, payload) => state.flDelivered(payload.get) })
      case FLLSend(`from`, `to`) => new Effect({ case (state, payload) => state.canflSend(payload.get) }, { case (state, _) => state })
    }
    Steps.steps(transition)
  }
}

object PruebaLinks extends App {

  val link01 = FairLossLink(0, 1)
  val initialState01 = FairLossLinkState(Set.empty)

  val link10 = FairLossLink(1, 0)
  val initialState10 = FairLossLinkState(Set.empty)


  val stubborn01 = StubbornLink(0, 1)
  val initalStateSL01 = StubbornLinkState(Set.empty, Set.empty)

  val stubborn10 = StubbornLink(1, 0)
  val initalStateSL10 = StubbornLinkState(Set.empty, Set.empty)


  val automaton: Option[Automaton[(((FairLossLinkState, FairLossLinkState), StubbornLinkState), StubbornLinkState)]] = for {
    c1 <- link01.composeTuple(link10)
    c2 <- c1.composeTuple(stubborn01)
    c3 <- c2.composeTuple(stubborn10)
  } yield (c3)

  //FairLossLink(0,1).steps.isDefinedAt((FairLossLinkState(Set.empty),Do(SLSend)))

  println(Execution.execute(automaton.get, (((initialState01, initialState10), initalStateSL01), initalStateSL10), Do(SLSend(0, 1), Some(1)))
    .next(Do(FLLSend(0, 1), Some(1)))
    .next(Do(FLLDeliver(0, 1), Some(1)))
    .next(Do(SLDeliver(0, 1), Some(1)))
    .state)

}
