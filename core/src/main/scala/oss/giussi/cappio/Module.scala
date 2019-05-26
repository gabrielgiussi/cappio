package oss.giussi.cappio

import oss.giussi.cappio.impl.net.FairLossLink.FLLSend
import oss.giussi.cappio.impl.net.Socket

object NextState {
  def apply[Req,State,Ind](module: Module[Req,State,Ind]): NextState[Req,State,Ind] = NextState[Req,State,Ind](Set.empty[Ind], Set.empty[FLLSend],module)

}

// TODO indications: Set[Ind], send: Set[FLLSend] -> Triggers para reusarlo

case class NextState[Req,State,Ind](indications: Set[Ind], send: Set[FLLSend], module: Module[Req,State,Ind])

trait Module[Req,State,Ind] {

  type Self = Module[Req,State,Ind]

  type Next = NextState[Req,State,Ind]

  def request(in: Req): Next

  def state: State

  protected def next(module: Module[Req,State,Ind],indications: Set[Ind] = Set.empty, send: Set[FLLSend] = Set.empty): Next = NextState(indications,send,module)

  def tail: Socket[Req,State,Ind]

  def tick: Next

}
