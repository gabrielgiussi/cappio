package oss.ggiussi.cappio.core

import oss.ggiussi.cappio.ProcessID
import oss.ggiussi.cappio.impl.links.{Envelope, Message}

object LinkProtocol {

  object Send {
    def apply(message: Message) = new Send(message.id.from, message.id.to, message)
  }

  object Deliver {
    def apply(message: Message) = new Deliver(message.id.from, message.id.to, message)
  }

  case class Send(from: ProcessID, to: ProcessID, message: Message) extends Envelope {
    override def toString: String = s"Send(from: $from, to: $to, payload: ${message.payload}, step: ${message.id.step})"
  }

  case class Deliver(from: ProcessID, to: ProcessID, message: Message) extends Envelope {
    override def toString: String = s"Deliver(from: $from, to: $to, payload: ${message.payload}, step: ${message.id.step})"
  }

}
