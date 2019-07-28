package oss.giussi.cappio

import oss.giussi.cappio.impl.net.FairLossLink.FLLSend
import oss.giussi.cappio.impl.net.Socket

object NextState {
  //def apply[Req,State,Ind](module: Module[Req,State,Ind]): NextState[Req,State,Ind] = NextState[Req,State,Ind](Set.empty[Ind], Set.empty[FLLSend],module)

}

// TODO indications: Set[Ind], send: Set[FLLSend] -> Triggers para reusarlo

case class NextState[M <: Mod](indications: Set[M#Ind], send: Set[FLLSend[M#Payload]], module: Module[M])

trait Mod {
  type Req
  type State
  type Ind
  type Payload
}

trait Module[M <: Mod] { // TODO Mod as TypeMember??

  //type SelfMod = M

  //type Self = Module[M] shapeless se va a la mierda con esto (stackoverflow)

  type Next = NextState[M]

  def request(in: M#Req): Next

  def state: M#State

  protected def next(module: Module[M],indications: Set[M#Ind] = Set.empty, send: Set[FLLSend[M#Payload]] = Set.empty): Next = new NextState(indications,send,module)

  def tail: Socket[M]

  def tick: Next

}
