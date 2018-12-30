package oss.ggiussi.cappio.core

import java.util.UUID

import oss.ggiussi.cappio.{InstanceID, ProcessID}

object LinkProtocol {

  case class MessageID(uuid: UUID)

  object Message {
    def apply(payload: Any): Message = new Message(payload, MessageID(UUID.randomUUID()))
  }

  case class Message(payload: Any, id: MessageID)

  case class SendHeader(from: ProcessID, to: ProcessID, instance: InstanceID) extends ActionHeader

  case class DeliverHeader(from: ProcessID, to: ProcessID, instance: InstanceID) extends ActionHeader

  // Todos los links van a accionar con este header, la alternativa es poner el from y to.
  case class DropHeader(from: ProcessID, to: ProcessID, instance: InstanceID) extends ActionHeader

  trait NetworkAction extends Action {
    def id: MessageID
  }

  case class Deliver(header: DeliverHeader, msg: Message) extends NetworkAction {
    override def payload = Some(msg)

    override def id: MessageID = msg.id
  }

  case class Send(header: SendHeader, msg: Message) extends NetworkAction {
    override def payload = Some(msg)

    override def id: MessageID = msg.id
  }


  case class Drop(header: DropHeader, id: MessageID) extends NetworkAction {
    override def payload = Some(id)
  }

  def send(from: ProcessID, to: ProcessID, instanceID: InstanceID, message: Message, metadata: Option[Any] = None): Send = Send(SendHeader(from, to, instanceID), message)

  def deliver(from: ProcessID, to: ProcessID, message: Message, instanceID: InstanceID, metadata: Option[Any] = None): Deliver = Deliver(DeliverHeader(from, to, instanceID), message)

  def drop(from: ProcessID, to: ProcessID, instance: InstanceID, msg: MessageID, metadata: Option[Any] = None): Action = Drop(DropHeader(from, to, instance), msg)

}
