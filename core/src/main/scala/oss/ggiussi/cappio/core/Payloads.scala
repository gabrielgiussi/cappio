package oss.ggiussi.cappio.core

import oss.ggiussi.cappio.{InstanceID, ProcessID}
import oss.ggiussi.cappio.core.LinkProtocol.Drop
import oss.ggiussi.cappio.impl.links.{Message, deliver, send}

case class Payloads(payloads: Set[Any], steps: Int) {
  def drops(from: Int, to: Int)(implicit instance: InstanceID): Set[Action] = (for (p <- payloads; s <- 0 until steps) yield (p, s)).map { case (p, s) => Drop(Message(from,to,p,s).id,instance) }

  def sends(from: Int, to: Int)(implicit instance: InstanceID): Set[Action] = (for (p <- payloads; s <- 0 until steps) yield (p, s)).map { case (p, s) => send(from, to, p)(s) }

  def delivers(from: Int, to: Int)(implicit instance: InstanceID): Set[Action] = (for (p <- payloads; s <- 0 until steps) yield (p, s)).map { case (p, s) => deliver(from, to, p)(s) }

  def messages(from: ProcessID, to: ProcessID): Set[Message] = (for (p <- payloads; s <- 0 until steps) yield (p,s)).map { case (p,s) => Message(from,to,p,s)}

  def enabled(p: Any): Boolean = payloads contains p
}
