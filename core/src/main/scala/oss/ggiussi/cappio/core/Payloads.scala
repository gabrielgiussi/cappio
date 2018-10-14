package oss.ggiussi.cappio.core

import oss.ggiussi.cappio.ProcessID
import oss.ggiussi.cappio.impl.links.{Message, deliver, send}

case class Payloads(payloads: Set[Any], steps: Int) {
  def sends(from: Int, to: Int): Set[Action] = (for (p <- payloads; s <- 0 until steps) yield (p, s)).map { case (p, s) => send(from, to, p)(s) }

  def delivers(from: Int, to: Int): Set[Action] = (for (p <- payloads; s <- 0 until steps) yield (p, s)).map { case (p, s) => deliver(from, to, p)(s) }

  def messages(from: ProcessID, to: ProcessID): Set[Message] = (for (p <- payloads; s <- 0 until steps) yield (p,s)).map { case (p,s) => Message(from,to,p,s)}

  def enabled(p: Any): Boolean = payloads contains p
}
