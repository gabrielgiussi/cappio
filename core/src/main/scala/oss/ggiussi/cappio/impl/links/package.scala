package oss.ggiussi.cappio.impl

import oss.ggiussi.cappio.ProcessID
import oss.ggiussi.cappio.core.LinkProtocol._
import oss.ggiussi.cappio.core._

package object links {

  trait Envelope extends Action

  case class MessageID(from: ProcessID, to: ProcessID, payload: Any, step: Int)

  case class Message(payload: Any, id: MessageID)

  def send(from: ProcessID, to: ProcessID, payload: Any)(step: Int) = Send(from, to, Message(payload, MessageID(from, to, payload, step)))

  def deliver(from: ProcessID, to: ProcessID, payload: Any)(step: Int) = Deliver(from, to, Message(payload, MessageID(from, to, payload, step)))



}
