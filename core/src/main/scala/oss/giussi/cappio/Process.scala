package oss.giussi.cappio

import oss.giussi.cappio.impl.net.FairLossLink.FLLSend

case class NextStateProcess[M <: Mod](indications: Set[M#Ind], send: Set[FLLSend[M#Payload]], process: Process[M])

// TODO puedo poner la clase dentro de Process?
case class Process[M <: Mod](id: ProcessId, stack: Module[M], status: ProcessStatus = Up) {

  type Self = Process[M]

  type Next = NextStateProcess[M]

  def tick = next(stack.tick)

  def deliver(packet: FLLDeliver[M#Payload]): Next = next(stack.tail.deliver(packet))

  def request(r: M#Req): Next = next(stack.request(r))

  private def next(ns: NextState[M]): Next = NextStateProcess(ns.indications,ns.send,copy(stack = ns.module))


}
