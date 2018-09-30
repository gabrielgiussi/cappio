package oss.ggiussi.cappio.impl.links

import oss.ggiussi.cappio.Transition.Transition
import oss.ggiussi.cappio._
import oss.ggiussi.cappio.impl.links.Protocol.{FLLDeliver, Envelope, ProcessID, FLLSend}

object Protocol {
  type ProcessID = Int

  type MessageID = Int

  trait Envelope extends Action

  case class FLLSend(from: ProcessID, to: ProcessID) extends Envelope

  case class FLLDeliver(from: ProcessID, to: ProcessID) extends Envelope

  case class Message(id: MessageID, payload: Any)

  implicit class ArbiterProtocol(id: ProcessID) {

    def sendsTo(to: Int)(payload: Any): Do = Do(FLLSend(id, to), Some(payload))

    def deliversFrom(from: Int)(payload: Any) = Do(FLLDeliver(from, id), Some(payload))

  }
}


// TOOD should be something like Payload instead of Any (but Payloa dis Option[Any])
case class FairLossLinkState(messages: Set[Any]) {
  def add(message: Any): FairLossLinkState = copy(messages = messages + message)

  def remove(message: Any): FairLossLinkState = copy(messages = messages - message)

  def canDeliver(message: Any): Boolean = messages contains message
}

case class FairLossLink(from: ProcessID, to: ProcessID) extends Automaton[FairLossLinkState] {
  override val sig: ActionSignature = {
    import Protocol._
    val in: Set[Action] = Set(FLLSend(from,to))
    val out: Set[Action] = Set(FLLDeliver(from,to))
    val int: Set[Action] = Set.empty[Action]
    ActionSignature(in = in, out = out, int = int)
  }
  override val steps: Steps.Steps[FairLossLinkState] = {
    val transitions: Transition[FairLossLinkState] = {
      case FLLSend(`from`, `to`) => Effect.inputEnabledP({ case (state,payload) => payload.map(state.add(_)).getOrElse(state) })
      case FLLDeliver(`from`, `to`) => new Effect({ case (state,payload) => state.canDeliver(payload.get) },{ case (state,payload) => state.remove(payload.get) }) // payload.get
    }
    Steps.steps(transitions)
  }
}

case class Process(id: ProcessID, neighbors: Set[ProcessID]) extends Automaton[Int] {
  override val sig: ActionSignature = {
    //val out: Set[Action] = neighbors.map(Send(id,_))
    val in: Set[Action] = neighbors.map(FLLDeliver(_,id))
    ActionSignature(in = in, out = Set(), int = Set())
  }
  override val steps: Steps.Steps[Int] = {
    val transition: Transition[Int] = {
      case FLLDeliver(_,`id`) => Effect.inputEnabledP({ case (state,payload) => state + payload.get.asInstanceOf[Int] })
    }
    Steps.steps(transition)
  }
}

object Probando extends App {

  val link01 = FairLossLink(0,1)
  val initialState01 = FairLossLinkState(Set.empty)

  val link10 = FairLossLink(1,0)
  val initialState10 = FairLossLinkState(Set.empty)

  val process0 = Process(0,Set(1))
  val process1 = Process(1,Set(0))

  val automaton: Automaton[(((FairLossLinkState, FairLossLinkState), Int), Int)] = link01.composeTuple(link10).get.composeTuple(process0).get.composeTuple(process1).get

  println(Execution.execute(automaton,(((initialState01,initialState10),0),0),Do(FLLSend(0,1),Some(1)))
      .next(Do(FLLDeliver(0,1),Some(1)))
    .state)

}