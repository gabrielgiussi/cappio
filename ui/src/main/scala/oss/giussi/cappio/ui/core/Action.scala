package oss.giussi.cappio.ui.core

import java.util.UUID

import oss.giussi.cappio.ProcessId

sealed trait Action {
  def id: String // TODO improve concrete impls
}

case class Index(i: Int)

case class Crashed(process: ProcessId, index: Index) extends Action {
  override val id = s"crashed-$process-$index"
}

case class Request(process: ProcessId, index: Index, payload: String) extends Action {
  override def id: String = s"request-$process-$index"
}

sealed abstract class NetworkAction(from: ProcessId, to: ProcessId, uuid: UUID, payload: String, sent: Index) extends Action {

  override val id = s"network-f:$from-t:$to-$uuid-$sent"

}

case class Indication(process: ProcessId, index: Index, payload: String) extends Action {
  override def id: String = s"indication-$process-$index"
}

case class Undelivered(from: ProcessId, to: ProcessId, uuid: UUID, payload: String, sent: Index) extends NetworkAction(from,to,uuid,payload,sent)

case class Delivered(from: ProcessId, to:ProcessId, uuid: UUID, payload: String, sent: Index, received: Index) extends NetworkAction(from,to,uuid,payload,sent)

case class Dropped(from: ProcessId, to: ProcessId, uuid: UUID, payload: String, sent: Index, dropped: Index) extends NetworkAction(from,to,uuid,payload,sent)

case class Base(processes: List[ProcessId], rounds: Int)

case class Schedule(actions: Set[Action])