package oss.giussi.cappio.impl.net

import oss.giussi.cappio.impl.net.FairLossLink.FLLSend
import oss.giussi.cappio.impl.net.PerfectLink.{PLDeliver, PLSend}
import oss.giussi.cappio.{FLLDeliver, Module, NextState, Packet}


object SmartPerfectLinkState {

  def init(timeout: Int) = SmartPerfectLinkState(Set.empty,0,timeout)
}

case class SmartPerfectLinkState(undelivered: Set[Packet], timer: Int, timeout: Int) {
  def delivered(p: Packet) = copy(undelivered = undelivered - p)

  def send(p: Packet) = copy(undelivered = undelivered + p)

  def tick(): (SmartPerfectLinkState,Set[Packet]) = {
    if (timer + 1 == timeout) (copy(timer = 0), undelivered)
    else (copy(timer = timer + 1),Set.empty)
  }
}

object SmartPerfectLink {

  def init(timeout: Int) = SmartPerfectLink(SmartPerfectLinkState.init(timeout))
}

case class SmartPerfectLink(state: SmartPerfectLinkState) extends Module[PLSend, SmartPerfectLinkState, PLDeliver] with Socket[PLSend,SmartPerfectLinkState,PLDeliver] {
  override def request(in: PLSend): Next = {
    next(copy(state.send(in.packet)),send = Set(FLLSend(in.packet)))
  }

  override def tail: Socket[PLSend, SmartPerfectLinkState, PLDeliver] = this

  override def tick: Next = {
    val (ns,packets) = state.tick()
    next(copy(ns), send = packets.map(FLLSend))
  }

  override def deliver(d: FLLDeliver): NextState[PLSend, SmartPerfectLinkState, PLDeliver] = {
    next(copy(state.delivered(d.packet)), indications = Set(PLDeliver(d.packet)))
  }
}
