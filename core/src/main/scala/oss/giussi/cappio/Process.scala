package oss.giussi.cappio

import oss.giussi.cappio.impl.net.FairLossLink.{FLLDeliver, FLLSend}

case class NextStateProcess[Req,State,Ind](indications: Set[Ind], send: Set[FLLSend], process: Process[Req,State,Ind])

// TODO puedo poner la clase dentro de Process?
case class Process[IN,State,Out](id: ProcessId, stack: Module[IN,State,Out], status: ProcessStatus = Up) {

  type Self = Process[IN,State,Out]

  type Next = NextStateProcess[IN,State,Out]

  def tick() = next(stack.tick)

  def deliver(packet: FLLDeliver): Next = next(stack.tail.deliver(packet))

  def request(r: IN): Next = next(stack.request(r))

  private def next(ns: NextState[IN,State,Out]): Next = NextStateProcess(ns.indications,ns.send,copy(stack = ns.module))


}
