package oss.ggiussi.cappio.core

import oss.ggiussi.cappio.{InstanceID, ProcessID}
import oss.ggiussi.cappio.impl.links.{Envelope, Message, MessageID}

object LinkProtocol {

  object Send {
    def apply(message: Message)(implicit instanceID: InstanceID) = new Send(message.id.from, message.id.to, instanceID, message)
  }

  object Deliver {
    def apply(message: Message)(implicit instanceID: InstanceID) = new Deliver(message.id.from, message.id.to, instanceID, message)
  }

  case class Send(from: ProcessID, to: ProcessID, instance: InstanceID, message: Message) extends Envelope {
    override def toString: String = s"Send(from: $from, to: $to, payload: ${message.payload}, step: ${message.id.step}, instance: ${instance.id})"
  }

  case class Deliver(from: ProcessID, to: ProcessID,instance: InstanceID, message: Message) extends Envelope {
    override def toString: String = s"Deliver(from: $from, to: $to, payload: ${message.payload}, step: ${message.id.step})"
  }

  case class Drop(m: MessageID, instance: InstanceID) extends Action

}
