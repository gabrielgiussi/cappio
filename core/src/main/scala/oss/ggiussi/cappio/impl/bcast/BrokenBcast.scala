package oss.ggiussi.cappio.impl.bcast

import oss.ggiussi.cappio.{InstanceID, ProcessID}
import oss.ggiussi.cappio.core.LinkProtocol.{Deliver, Send}
import oss.ggiussi.cappio.core.Transition.Transition
import oss.ggiussi.cappio.core._
import oss.ggiussi.cappio.impl.Instances
import oss.ggiussi.cappio.impl.bcast.BrokenBcastProtocol._
import oss.ggiussi.cappio.impl.links.Message

object BrokenBcastProtocol {

  // TODO tambien podria ser que BrkBcast ya tenga la lista de Message to deliver.
  // step here seems odd
  case class BrkBcast(from: ProcessID, instance: InstanceID, payload: Any, step: Int) extends Action {
    override def toString: String = s"$from Bcast $payload"
  }

  case class BrkDeliver(from: ProcessID, to: ProcessID,instance: InstanceID, message: Message) extends Action // <----- process now must understand BrkDeliver instead of Deliver!!!! (should create a parametrizable Process)?

}

object BrokenBcastState {
  def empty() = BrokenBcastState(messages = Set.empty, delivered = Set.empty)
}

case class BrokenBcastState(messages: Set[Message], delivered: Set[Message]) {

  def bcast(msgs: Set[Message]): BrokenBcastState = copy(messages = messages ++ msgs)

  def deliver(message: Message): BrokenBcastState = copy(delivered = delivered + message)


  def canSend(message: Message): Boolean = messages contains message

  def canBDeliver(message: Message): Boolean = delivered contains message

  def bDeliver(message: Message): BrokenBcastState = copy(delivered = delivered - message)

  def send(message: Message): BrokenBcastState = copy(messages = messages - message)
}

object BrokenBcast {
  def apply(instance: InstanceID)(id: ProcessID, neighbors: Set[ProcessID])(implicit payloads: Payloads): BrokenBcast = new BrokenBcast(id,neighbors,instance)(payloads)
}

// A BebBcast will look like the same, the only thing that changes is the implementation of the link
case class BrokenBcast(id: ProcessID, neighbors: Set[ProcessID], instance: InstanceID)(implicit payloads: Payloads) extends Automaton[BrokenBcastState] {

  import Instances._

  private val processes = neighbors + id

  override val sig: ActionSignature = {
    val messages = processes.flatMap(p => payloads.messages(id, p))
    val in: Set[Action] = {
      val delivers: Set[Action] = messages.map(Deliver(_)(BCAST_LINK))
      val bcast: Set[Action] = messages.map(m => BrkBcast(m.id.from,instance, m.payload, m.id.step)) // TODO
      delivers ++ bcast
    }
    val out: Set[Action] = {
      val sends: Set[Action] = messages.map(Send(_)(BCAST_LINK))
      val bDelivers: Set[Action] = messages.map(m => BrkDeliver(m.id.from, m.id.to,instance, m))
      sends ++ bDelivers
    }
    val int: Set[Action] = Set.empty[Action]
    ActionSignature(in = in, out = out, int = int)
  }

  override val steps: Steps.Steps[BrokenBcastState] = Steps.steps[BrokenBcastState]({
    case BrkBcast(`id`, `instance`, payload, step) if payloads enabled payload => Effect(_.bcast(processes.map(to => Message(from = id, to = to, payload = payload, step))))
    case BrkDeliver(`id`, to,`instance`, msg) if (processes contains to) && (payloads enabled msg.payload) => Effect(_.canBDeliver(msg), _.bDeliver(msg))
    case Send(`id`, to, `BCAST_LINK`, msg) if processes contains to => Effect(_.canSend(msg), _.send(msg))
    case Deliver(`id`, to, `BCAST_LINK`, msg) if processes contains to => Effect(_.deliver(msg))
  })
}
