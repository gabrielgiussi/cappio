package oss.ggiussi.cappio.impl

import oss.ggiussi.cappio.{InstanceID, ProcessID}
import oss.ggiussi.cappio.core.LinkProtocol._
import oss.ggiussi.cappio.core._

package object links {

  trait Envelope extends Action

  case class MessageID(from: ProcessID, to: ProcessID, payload: Any, step: Int)

  object Message {

    def apply(from: ProcessID, to: ProcessID, payload: Any, step: Int): Message = new Message(payload, MessageID(from, to, payload, step))

  }

  case class Message(payload: Any, id: MessageID)

  def send(from: ProcessID, to: ProcessID, payload: Any)(step: Int)(implicit instance: InstanceID) = Send(Message(from, to, payload, step))

  def deliver(from: ProcessID, to: ProcessID, payload: Any)(step: Int)(implicit instance: InstanceID) = Deliver(Message(from, to, payload, step))


}
