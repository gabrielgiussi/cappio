package oss.ggiussi.cappio.impl.bcast

import com.sun.xml.internal.ws.api.pipe.NextAction
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

  case class BrkDeliver(from: ProcessID, to: ProcessID, instance: InstanceID, message: Message) extends Action // <----- process now must understand BrkDeliver instead of Deliver!!!! (should create a parametrizable Process)?

}

object BrokenBcastState {
  def empty() = BrokenBcastState(toSend = Set.empty, toDeliver = Set.empty)
}

case class BrokenBcastState(toSend: Set[Send], toDeliver: Set[BrkDeliver]) {
  def bcast(from: ProcessID, processes: Set[ProcessID], payload: Any, step: Int) = {
    val _toSend = processes.map(to => Send(from, to, Instances.BCAST_LINK, Message(from, to, payload, step)))
    NextState(copy(toSend = toSend ++ _toSend), Set.empty[Action] ++ _toSend)
  }

  def canSend(s: Send): Boolean = toSend contains s

  def send(s: Send) = copy(toSend = toSend - s)

  def canBDeliver(d: BrkDeliver): Boolean = toDeliver contains d

  def bDeliver(d: BrkDeliver) = copy(toDeliver = toDeliver - d)

  def deliver(msg: Message) = {
    val _toDeliver = BrkDeliver(msg.id.from, msg.id.to, Instances.BCAST, msg)
    NextState(copy(toDeliver = toDeliver + _toDeliver), Set(_toDeliver))
  }
}


object BrokenBcast {
  def apply(instance: InstanceID)(id: ProcessID, neighbors: Set[ProcessID])(implicit payloads: Payloads): BrokenBcast = new BrokenBcast(id, neighbors, instance)(payloads)
}

// A BebBcast will look like the same, the only thing that changes is the implementation of the link
case class BrokenBcast(id: ProcessID, neighbors: Set[ProcessID], instance: InstanceID)(implicit payloads: Payloads) extends Automaton[BrokenBcastState] {

  import Instances._

  private val processes = neighbors + id

  override val sig: ActionSignature = {
    val messages = processes.flatMap(p => payloads.messages(id, p))
    val in: Set[Action] = {
      val delivers: Set[Action] = messages.map(Deliver(_)(BCAST_LINK))
      val bcast: Set[Action] = messages.map(m => BrkBcast(m.id.from, instance, m.payload, m.id.step)) // TODO
      delivers ++ bcast
    }
    val out: Set[Action] = {
      val sends: Set[Action] = messages.map(Send(_)(BCAST_LINK))
      val bDelivers: Set[Action] = messages.map(m => BrkDeliver(m.id.from, m.id.to, instance, m))
      sends ++ bDelivers
    }
    val int: Set[Action] = Set.empty[Action]
    ActionSignature(in = in, out = out, int = int)
  }

  override val steps: Steps.Steps[BrokenBcastState] = Steps.steps[BrokenBcastState]({
    case BrkBcast(`id`, `instance`, payload, step) if payloads enabled payload => Effect.triggers(_.bcast(id, processes, payload, step))
    case a@BrkDeliver(`id`, to,`instance`, msg) if (processes contains to) && (payloads enabled msg.payload) => Effect(_.canBDeliver(a), _.bDeliver(a))
    case a@Send(`id`, to, `BCAST_LINK`, msg) if processes contains to => Effect(_.canSend(a), _.send(a))
    case Deliver(`id`, to, `BCAST_LINK`, msg) if processes contains to => Effect.triggers(_.deliver(msg))
  })
}
