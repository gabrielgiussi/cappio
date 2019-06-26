package oss.giussi.cappio

import oss.giussi.cappio.impl.net.FairLossLink.FLLSend

// como hago para que IN tenga una NoOp?

case class Crash(processId: ProcessId)

object Scheduler {

  def init[In,State,Out](processes: List[Process[In,State,Out]]) = Scheduler(processes.map(p => p.id -> p).toMap, Network.init(), 0)
}

case class RequestBatch[Req](requests: Map[ProcessId,Req]) {
  def add(processId: ProcessId, req: Req): RequestBatch[Req] = copy(requests = requests + (processId -> req))

  def remove(processId: ProcessId) = copy(requests = requests - processId)
}

object DeliverBatch {

  val empty: DeliverBatch = DeliverBatch(Map.empty)
}

case class DeliverBatch(ops: Map[ProcessId,Either[FLLDeliver,Drop]]) {
  def add(deliver: FLLDeliver) = copy(ops = ops + (deliver.packet.to -> Left(deliver)))

  def add(drop: Drop) = copy(ops = ops + (drop.packet.to -> Right(drop)))

  def remove(deliver: FLLDeliver) = copy(ops = ops - deliver.packet.to)

  def remove(drop: Drop) = copy(ops = ops - drop.packet.to)

  def clear() = DeliverBatch.empty
}

sealed trait Step[Req,State,Ind] // TODO delete? esta bien hacer traits con generics q no usa?

case class WaitingRequest[Req,State,Ind](scheduler: TickScheduler[Req,State,Ind]) extends Step[Req,State,Ind] {
  def request(batch: RequestBatch[Req]) = {
    val NextStateTickScheduler(sent,ind,sch) = scheduler.request(batch)
    (sent, ind,WaitingDeliver(sch))
  }
}
case class WaitingDeliver[Req,State,Ind](scheduler: TickScheduler[Req,State,Ind]) extends Step[Req,State,Ind] {
  def deliver(packets: DeliverBatch) = {
    val NextStateTickScheduler(sent,ind,sch) = scheduler.deliver(packets)
    (sent,ind,WaitingRequest(sch))
  }
}

// FIXME estas dos clases son un asco
case class NextStateScheduler[Req,State,Ind](sent: Set[FLLSend], indications: Set[Ind], scheduler: Scheduler[Req,State,Ind])

case class NextStateTickScheduler[Req,State,Ind](sent: Set[FLLSend], indications: Set[Ind], scheduler: TickScheduler[Req,State,Ind])

case class TickScheduler[IN, State, Out](scheduler: Scheduler[IN,State,Out]){

  def request(batch: RequestBatch[IN]) = {
    val NextStateScheduler(sent0,ind0,sch0) = scheduler.request(batch)
    val NextStateScheduler(sent1,ind1,sch1) = sch0.tick()
    NextStateTickScheduler(sent0 ++ sent1, ind0 ++ ind1,copy(sch1))
  }

  def deliver(delivers: DeliverBatch) = {
    val NextStateScheduler(sent0,ind0,sch0) = scheduler.deliver(delivers)
    val NextStateScheduler(sent1,ind1,sch1) = sch0.tick()
    NextStateTickScheduler(sent0 ++ sent1, ind0 ++ ind1,copy(sch1))
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

    NextStateScheduler(fs, fi,copy(processes = processes ++ fp.map(p => p.id -> p), network = network.send(fs)))
  }

  def tick(): Next = {
    val (fi,fs,fp) = processes.values.foldLeft[(Set[Out],Set[FLLSend],Set[P])]((Set.empty,Set.empty,Set.empty)) {
      case ((ind,send,ps),p) =>
        val NextStateProcess(i,s,ns) = p.tick
        (ind ++ i,send ++ s,ps + ns)
    }
    NextStateScheduler(fs, fi,copy(processes = fp.map(p => p.id -> p).toMap, network = network.send(fs), step = step + 1))
  }

  def deliver(ds: DeliverBatch) = {
    def deliver(d: FLLDeliver) = processes(d.packet.to).deliver(d)
    val d = ds.ops.values.collect { case Left(value) => value }.toSet
    val drops = ds.ops.values.collect { case Right(value) => value }

    val nn = network.deliver(d).flatMap(_.drop(drops.map(_.packet).toSet)).get
    println(d)

    val (fi,fs,fp) = d.foldLeft[(Set[Out],Set[FLLSend],Set[P])]((Set.empty,Set.empty,Set.empty)) {
      case ((ind,send,ps),d) =>
        val NextStateProcess(i,s,ns) = deliver(d)
        (ind ++ i,send ++ s,ps + ns)
    }
    NextStateScheduler(fs, fi,copy(processes = processes ++ fp.map(p => p.id -> p).toMap, network = nn.send(fs)))
  }

}
