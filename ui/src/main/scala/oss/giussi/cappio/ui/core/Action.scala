package oss.giussi.cappio.ui.core

import java.util.UUID

import oss.giussi.cappio.ProcessId

sealed trait Action {
  def id: String // TODO improve concrete impls

  def tags: Set[ActionTag] = Set.empty

  def payload: Option[String] = None // TODO this or pattern matching to see if it has payload?
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

case class Request(name: String, process: ProcessId, index: Index, _payload: String) extends Action {
  override def id: String = s"request-$process-$index"

  override def payload: Option[String] = Some(_payload)
}

sealed abstract class NetworkAction(val _from: ProcessId, val _to: ProcessId, val _uuid: UUID, val _payload: String, val _sent: Index, override val tags: Set[ActionTag]) extends Action {

  override val id = s"network-f:${_from}-t:${_to}-${_uuid}-${_sent}"

  def from: ProcessId

  def to: ProcessId

  override def payload: Option[String] = Some(_payload)

  def uuid: UUID

  def sent: Index

}

case class Indication(process: ProcessId, index: Index, _payload: String) extends Action {
  override def id: String = s"indication-$process-$index"

  override def payload: Option[String] = Some(_payload)
}

// TODO alreadyDelivered should be a TAG?
case class Undelivered(from: ProcessId, to: ProcessId, uuid: UUID, p: String, sent: Index, alreadyDelivered: Boolean, _tags: Set[ActionTag]) extends NetworkAction(from, to, uuid, p, sent, _tags)

object Actions {

  def undelivered(from: ProcessId, to: ProcessId, uuid: UUID, payload: String, sent: Index, alreadyDelivered: Boolean, tags: Set[ActionTag] = Set.empty): Undelivered = Undelivered(from, to, uuid, payload, sent, alreadyDelivered, tags)

  def delivered(from: ProcessId, to: ProcessId, uuid: UUID, payload: String, sent: Index, received: Index, tags: Set[ActionTag] = Set.empty): Delivered = Delivered(from, to, uuid, payload, sent, received, tags)


}

case class Delivered(from: ProcessId, to: ProcessId, uuid: UUID, p: String, sent: Index, received: Index, _tags: Set[ActionTag]) extends NetworkAction(from, to, uuid, p, sent, _tags)

case class Dropped(from: ProcessId, to: ProcessId, uuid: UUID, p: String, sent: Index, dropped: Index) extends NetworkAction(from, to, uuid, p, sent,Set.empty)

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