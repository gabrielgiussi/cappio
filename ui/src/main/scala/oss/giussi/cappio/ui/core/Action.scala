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

sealed abstract class NetworkAction(val _from: ProcessId,val _to: ProcessId,val _uuid: UUID,val _payload: String,val _sent: Index) extends Action {

  override val id = s"network-f:${_from}-t:${_to}-${_uuid}-${_sent}"

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

sealed abstract class ReadAction(process: ProcessId, index: Index) extends Action {
  override def id: String = s"rd-p:$process-i:$index"
}

sealed abstract class WriteAction(process: ProcessId, index: Index) extends Action {
  override def id: String = s"wr-p:$process-i:$index"
}

case class PendingRead(process: ProcessId, start: Index) extends ReadAction(process,start)

// typed value
case class ReadReturned(process: ProcessId, start: Index,returned: Index, value: String) extends ReadAction(process,start)

case class PendingWrite(process: ProcessId, start: Index, value: String) extends WriteAction(process,start)

case class WriteReturned(process: ProcessId, start: Index, returned: Index, value: String) extends WriteAction(process,start)
