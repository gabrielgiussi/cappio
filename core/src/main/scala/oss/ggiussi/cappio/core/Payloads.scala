package oss.ggiussi.cappio.core

import oss.ggiussi.cappio.impl.links.{deliver, send}

case class Payloads(payloads: Set[Any], steps: Int) {
  def sends(from: Int, to: Int): Set[Action] = (for (p <- payloads; s <- 0 until steps) yield (p, s)).map { case (p, s) => send(from, to, p)(s) }

  def delivers(from: Int, to: Int): Set[Action] = (for (p <- payloads; s <- 0 until steps) yield (p, s)).map { case (p, s) => deliver(from, to, p)(s) }
}
