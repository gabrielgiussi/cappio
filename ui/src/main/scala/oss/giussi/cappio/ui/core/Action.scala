package oss.giussi.cappio.ui.core

import java.util.UUID

import com.raquo.laminar.nodes.ReactiveHtmlElement
import oss.giussi.cappio.ProcessId

sealed trait Action {
  def id: String // TODO improve concrete impls

  def tags: Set[ActionTag] = Set.empty
}

sealed trait ActionTag

case object HEARTBEAT extends ActionTag

case object BCAST extends ActionTag

case class Index(i: Int) {
  def next = Index(i + 1)
}

case class Crashed(process: ProcessId, index: Index) extends Action {
  override val id = s"crashed-$process-$index"
}

case class Request(name: String, process: ProcessId, index: Index, payload: String, override val tags: Set[ActionTag]) extends Action {
  override def id: String = s"request-$process-$index"
}

sealed abstract class NetworkAction(val _from: ProcessId, val _to: ProcessId, val _uuid: UUID, val payload: String, val _sent: Index, override val tags: Set[ActionTag]) extends Action {

  override val id = s"network-f:${_from}-t:${_to}-${_uuid}-${_sent}"

  def from: ProcessId

  def to: ProcessId

  def uuid: UUID

  def sent: Index

}

case class Indication(name: String, process: ProcessId, index: Index, payload: String, override val tags: Set[ActionTag]) extends Action {
  override def id: String = s"indication-$process-$index"

}

// TODO alreadyDelivered should be a TAG?
case class Undelivered(from: ProcessId, to: ProcessId, uuid: UUID, p: String, sent: Index, alreadyDelivered: Boolean, _tags: Set[ActionTag]) extends NetworkAction(from, to, uuid, p, sent, _tags)

object Actions {

  import com.raquo.laminar.api.L._
  import org.scalajs.dom.raw.HTMLDivElement

  def request(name: String, process: ProcessId, index: Index, payload: String, tags: Set[ActionTag] = Set.empty): Request = Request(name, process, index, payload, tags)

  def indication(name: String, process: ProcessId, index: Index, payload: String, tags: Set[ActionTag] = Set.empty): Indication = Indication(name, process, index, payload, tags)

  def undelivered(from: ProcessId, to: ProcessId, uuid: UUID, payload: String, sent: Index, alreadyDelivered: Boolean, tags: Set[ActionTag] = Set.empty): Undelivered = Undelivered(from, to, uuid, payload, sent, alreadyDelivered, tags)

  def delivered(from: ProcessId, to: ProcessId, uuid: UUID, payload: String, sent: Index, received: Index, tags: Set[ActionTag] = Set.empty): Delivered = Delivered(from, to, uuid, payload, sent, received, tags)

  def showAction(action: Action): ReactiveHtmlElement[HTMLDivElement] = {
    def showAction(name: String, process: ProcessId, payload: String) = div(
      div(s"$name en $process"),
      div(s"Payload: $payload")
    )

    def showNetworkAction(uuid: UUID, from: ProcessId, to: ProcessId, status: String, payload: String) = div(
      div(s"Mensaje enviado de $from a $to"),
      div(s"Identificador: $uuid"),
      div(s"Payload: $payload"),
      div(s"Estado: $status")
    )

    action match {
      case Undelivered(from, to, uuid, p, _, _, _) => showNetworkAction(uuid, from, to, "Pendiente", p)
      case Delivered(from, to, uuid, p, _, _, _) => showNetworkAction(uuid, from, to, "Entregado", p)
      case Request(name, process, _, payload, _) => showAction(name, process, payload)
      case Indication(name, process, _, payload, _) => showAction(name, process, payload)
      case _ => div()
    }
  }

}

case class Delivered(from: ProcessId, to: ProcessId, uuid: UUID, p: String, sent: Index, received: Index, _tags: Set[ActionTag]) extends NetworkAction(from, to, uuid, p, sent, _tags)

case class Dropped(from: ProcessId, to: ProcessId, uuid: UUID, p: String, sent: Index, dropped: Index) extends NetworkAction(from, to, uuid, p, sent, Set.empty)

/*
sealed abstract class ReadAction(process: ProcessId, index: Index) extends Action {
  override def id: String = s"rd-p:$process-i:$index"
}

sealed abstract class WriteAction(process: ProcessId, index: Index) extends Action {
  override def id: String = s"wr-p:$process-i:$index"
}

case class PendingRead(process: ProcessId, start: Index) extends ReadAction(process, start)

case class ReadReturned(process: ProcessId, start: Index, returned: Index, value: String) extends ReadAction(process, start)

case class PendingWrite(process: ProcessId, start: Index, value: String) extends WriteAction(process, start)

case class WriteReturned(process: ProcessId, start: Index, returned: Index, value: String) extends WriteAction(process, start)
 */
