package oss.giussi.cappio.ui.core

import java.util.UUID

import oss.giussi.cappio.ProcessId

sealed trait Action {
  def id: String // TODO improve concrete impls
}

/*
sealed abstract case class Index(value: Int)

object Nat {
  implicit def asInt(index: Index): Int = index.value

  def fromInt(n: Int): Option[Index] =
    if (n >= 0) Some(new Index(n) {}) else None
}

 */

case class Index(i: Int)

case class Crashed(process: ProcessId, index: Index) extends Action {
  override val id = s"crashed-$process-$index"
}

case class Request(process: ProcessId, index: Index, payload: String) extends Action {
  override def id: String = s"request-$process-$index"
}

sealed abstract class NetworkAction(val _from: ProcessId,val _to: ProcessId, _uuid: UUID, _payload: String, _sent: Index) extends Action {

  override val id = s"network-f:$from-t:$to-$uuid-$sent"

  def from: ProcessId

  def to: ProcessId

  def payload: String

  def uuid: UUID

  def sent: Index

}

case class Indication(process: ProcessId, index: Index, payload: String) extends Action {
  override def id: String = s"indication-$process-$index"
}

case class Undelivered(from: ProcessId, to: ProcessId, uuid: UUID, payload: String, sent: Index) extends NetworkAction(from,to,uuid,payload,sent)

case class Delivered(from: ProcessId, to:ProcessId, uuid: UUID, payload: String, sent: Index, received: Index) extends NetworkAction(from,to,uuid,payload,sent)

case class Dropped(from: ProcessId, to: ProcessId, uuid: UUID, payload: String, sent: Index, dropped: Index) extends NetworkAction(from,to,uuid,payload,sent)
