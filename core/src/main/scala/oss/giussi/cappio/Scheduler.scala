package oss.giussi.cappio

import oss.giussi.cappio.impl.net.FairLossLink.{FLLDeliver, FLLSend}

// como hago para que IN tenga una NoOp?

case class Crash(processId: ProcessId)

object Scheduler {

  def init[In,State,Out](processes: List[Process[In,State,Out]]) = Scheduler(processes.map(p => p.id -> p).toMap, Network.init(), 0)
}

case class RequestBatch[Req](requests: Map[ProcessId,Req])

case class Drop(packet: Packet)

case class DeliverBatch(ops: Map[ProcessId,Either[FLLDeliver,Drop]])

sealed trait Step[Req,State,Ind] // TODO delete?

case class WaitingRequest[Req,State,Ind](scheduler: TickScheduler[Req,State,Ind]) extends Step[Req,State,Ind] {
  def request(batch: RequestBatch[Req]) = {
    val NextStateTickScheduler(ind,sch) = scheduler.request(batch)
    (ind,WaitingDeliver(sch))
  }
}
case class WaitingDeliver[Req,State,Ind](scheduler: TickScheduler[Req,State,Ind]) extends Step[Req,State,Ind] {
  def deliver(packets: DeliverBatch) = {
    val NextStateTickScheduler(ind,sch) = scheduler.deliver(packets)
    (ind,WaitingRequest(sch))
  }
}

// FIXME estas dos clases son un asco
case class NextStateScheduler[Req,State,Ind](indications: Set[Ind], scheduler: Scheduler[Req,State,Ind])

case class NextStateTickScheduler[Req,State,Ind](indications: Set[Ind], scheduler: TickScheduler[Req,State,Ind])

case class TickScheduler[IN, State, Out](scheduler: Scheduler[IN,State,Out]){

  def request(batch: RequestBatch[IN]) = {
    val NextStateScheduler(ind0,sch0) = scheduler.request(batch)
    val NextStateScheduler(ind1,sch1) = sch0.tick()
    NextStateTickScheduler(ind0 ++ ind1,copy(sch1))
  }

  def deliver(delivers: DeliverBatch) = {
    val NextStateScheduler(ind0,sch0) = scheduler.deliver(delivers)
    val NextStateScheduler(ind1,sch1) = sch0.tick()
    NextStateTickScheduler(ind0 ++ ind1,copy(sch1))
  }
}



case class Scheduler[IN, State, Out](processes: Map[ProcessId, Process[IN, State, Out]], network: Network, step: Int) {

  type Self = Scheduler[IN, State, Out]

  type P = Process[IN, State, Out]

  type Next = NextStateScheduler[IN,State,Out]

  // TODO aca mandar los crash?, o un step para crashes?
  def request(batch: RequestBatch[IN]) = {
    val (fi,fs,fp) = batch.requests.foldLeft[(Set[Out],Set[FLLSend],Set[P])]((Set.empty,Set.empty,Set.empty)){
      case ((ind,send,ps),(pid,req)) =>
        val NextStateProcess(i,s,ns) = processes(pid).request(req)
        (ind ++ i,send ++ s,ps + ns)
    }

    NextStateScheduler(fi,copy(processes = processes ++ fp.map(p => p.id -> p), network = network.send(fs)))
  }

  def tick(): Next = {
    val (fi,fs,fp) = processes.values.foldLeft[(Set[Out],Set[FLLSend],Set[P])]((Set.empty,Set.empty,Set.empty)) {
      case ((ind,send,ps),p) =>
        val NextStateProcess(i,s,ns) = p.tick
        (ind ++ i,send ++ s,ps + ns)
    }
    NextStateScheduler(fi,copy(processes = fp.map(p => p.id -> p).toMap, network = network.send(fs), step = step + 1))
  }

  def deliver(ds: DeliverBatch) = {
    def deliver(d: FLLDeliver) = processes(d.packet.to).deliver(d)
    val d = ds.ops.values.collect { case Left(value) => value }.toSet
    val drops = ds.ops.values.collect { case Right(value) => value }

    val nn = network.deliver(d).flatMap(_.drop(drops.map(_.packet).toSet)).get

    val (fi,fs,fp) = d.foldLeft[(Set[Out],Set[FLLSend],Set[P])]((Set.empty,Set.empty,Set.empty)) {
      case ((ind,send,ps),d) =>
        val NextStateProcess(i,s,ns) = deliver(d)
        (ind ++ i,send ++ s,ps + ns)
    }
    NextStateScheduler(fi,copy(processes = processes ++ fp.map(p => p.id -> p).toMap, network = nn.send(fs)))
  }

}
