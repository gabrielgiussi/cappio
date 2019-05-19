package oss.giussi.cappio.impl.bcast

import oss.giussi.cappio.impl.bcast.BestEffortBroadcast.{BebBcast, BebDeliver}
import oss.giussi.cappio.impl.net.FairLossLink.{FLLDeliver, FLLSend}
import oss.giussi.cappio.impl.net.PerfectLink.{PLDeliver, PLSend, PerfectLinkState}
import oss.giussi.cappio.impl.net.Socket
import oss.giussi.cappio.{Instance, Module, NextState, Packet, ProcessId}

object BestEffortBroadcast {
  case class BebBcast(payload: Any, instance: Instance)

  case class BebDeliver(from: ProcessId, payload: Any)
}

case class BestEffortBroadcast(self: ProcessId, all: Set[ProcessId], pl: Module[PLSend,PerfectLinkState,PLDeliver]) extends Module[BebBcast,Unit,BebDeliver]{
  override def request(in: BebBcast): Next = {
    val (toSend,newpl) = all.map(to => PLSend(Packet(self,to,in.payload,in.instance)))
    .foldLeft((Set.empty[FLLSend],pl)){
      case ((sends,link),req) =>
        val NextState(_,s,plns) = link.request(req)
        (sends ++ s,plns)
    }
    next(copy(pl = newpl),send = toSend)
  }

  override def state: Unit = ()

  override def tail: Socket[BebBcast, Unit, BebDeliver] = (deliver: FLLDeliver) => {
    val NextState(ind,_,plns) = pl.tail.deliver(deliver)
    val PLDeliver(Packet(_,payload,from,_,_)) = ind.head // FIXME ind.head
    next(copy(pl = plns), indications = Set(BebDeliver(from,payload)))
  }

  override def tick: Next = {
    val NextState(_,send,plns) = pl.tick
    next(copy(pl = plns), send = send)
  }
}
