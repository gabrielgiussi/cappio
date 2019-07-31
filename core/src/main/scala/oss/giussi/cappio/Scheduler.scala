package oss.giussi.cappio

import oss.giussi.cappio.impl.net.FairLossLink.FLLSend

// como hago para que IN tenga una NoOp?

case class Crash(processId: ProcessId)

object Scheduler {

  def init[M <: Mod](processes: Set[Process[M]]) = Scheduler(processes.map(p => p.id -> p).toMap, Network.init[M#Payload](), 0)

  def init[M <: Mod](processes: Set[ProcessId], f: ProcessId => Process[M]) = Scheduler(processes.map(p => p -> f(p)).toMap, Network.init[M#Payload](), 0)
}

object RequestBatch {
  def empty[R] = new RequestBatch[R](Map.empty)

  def apply[R](requests: (ProcessId, R)*): RequestBatch[R] = new RequestBatch(requests.toMap)
}

case class RequestBatch[Req](requests: Map[ProcessId,Req]) {
  def add(processId: ProcessId, req: Req): RequestBatch[Req] = copy(requests = requests + (processId -> req))

  def remove(processId: ProcessId) = copy(requests = requests - processId)
}

object DeliverBatch {

  def empty[P]: DeliverBatch[P] = new DeliverBatch(Map.empty)

  def apply[P](ops: Either[FLLDeliver[P],Drop[P]]*): DeliverBatch[P] = new DeliverBatch(ops.map {
    case d@Left(FLLDeliver(p)) => p.to -> d
    case d@Right(Drop(p)) => p.to -> d
  }.toMap)
}

// deliver shoulnd't receive a map, just a list of delivers (the packet contains the target process)
case class DeliverBatch[P](ops: Map[ProcessId,Either[FLLDeliver[P],Drop[P]]]) {
  def add(deliver: FLLDeliver[P]) = copy(ops = ops + (deliver.packet.to -> Left(deliver)))

  def add(drop: Drop[P]) = copy(ops = ops + (drop.packet.to -> Right(drop)))

  def remove(deliver: FLLDeliver[P]) = copy(ops = ops - deliver.packet.to)

  def remove(drop: Drop[P]) = copy(ops = ops - drop.packet.to)

  def clear() = DeliverBatch.empty
}

sealed trait Step[M <: Mod] // TODO delete? esta bien hacer traits con generics q no usa?

case class RequestResult[M <: Mod](sent: Set[FLLSend[M#Payload]],ind: Set[IndicationFrom[M#Ind]], waiting: WaitingDeliver[M]){
  def deliver = waiting.deliver _
}

case class DeliverResult[M <: Mod](sent: Set[FLLSend[M#Payload]],ind: Set[IndicationFrom[M#Ind]], waiting: WaitingRequest[M]){
  def request = waiting.request _
}

case class WaitingRequest[M <: Mod](scheduler: TickScheduler[M]) extends Step[M] {
  def request(batch: RequestBatch[M#Req]) = {
    val NextStateTickScheduler(sent,ind,sch) = scheduler.request(batch)
    RequestResult(sent, ind,WaitingDeliver(sch))
  }
}
case class WaitingDeliver[M <: Mod](scheduler: TickScheduler[M]) extends Step[M] {
  def deliver(packets: DeliverBatch[M#Payload]) = {
    val NextStateTickScheduler(sent,ind,sch) = scheduler.deliver(packets)
    DeliverResult(sent,ind,WaitingRequest(sch))
  }
}

// FIXME estas dos clases son un asco
case class NextStateScheduler[M <: Mod](sent: Set[FLLSend[M#Payload]], indications: Set[IndicationFrom[M#Ind]], scheduler: Scheduler[M])

case class NextStateTickScheduler[M <: Mod](sent: Set[FLLSend[M#Payload]], indications: Set[IndicationFrom[M#Ind]], scheduler: TickScheduler[M])

case class TickScheduler[M <: Mod](scheduler: Scheduler[M]){

  def request(batch: RequestBatch[M#Req]) = {
    val NextStateScheduler(sent0,ind0,sch0) = scheduler.request(batch)
    val NextStateScheduler(sent1,ind1,sch1) = sch0.tick()
    NextStateTickScheduler(sent0 ++ sent1, ind0 ++ ind1,copy(sch1))
  }

  def deliver(delivers: DeliverBatch[M#Payload]) = {
    val NextStateScheduler(sent0,ind0,sch0) = scheduler.deliver(delivers)
    val NextStateScheduler(sent1,ind1,sch1) = sch0.tick()
    NextStateTickScheduler(sent0 ++ sent1, ind0 ++ ind1,copy(sch1))
  }

}

case class IndicationFrom[I](p: ProcessId, i: I)

case class Scheduler[M <: Mod](processes: Map[ProcessId, Process[M]], network: Network[M#Payload], step: Int) {

  type Self = Scheduler[M]

  type P = Process[M]

  type Next = NextStateScheduler[M]

  type Send = FLLSend[M#Payload]

  type Deliver = FLLDeliver[M#Payload]

  // TODO aca mandar los crash?, o un step para crashes?
  def request(batch: RequestBatch[M#Req]) = {
    val (fi,fs,fp) = batch.requests.foldLeft[(Set[IndicationFrom[M#Ind]],Set[Send],Set[P])]((Set.empty,Set.empty,Set.empty)){
      case ((ind,send,ps),(pid,req)) =>
        val NextStateProcess(i,s,ns) = processes(pid).request(req)
        val a = i.map(IndicationFrom(pid,_))
        (ind ++ a,send ++ s,ps + ns)
    }

    NextStateScheduler(fs, fi,copy(processes = processes ++ fp.map(p => p.id -> p), network = network.send(fs)))
  }

  def tick(): Next = {
    val (fi,fs,fp) = processes.values.foldLeft[(Set[IndicationFrom[M#Ind]],Set[Send],Set[P])]((Set.empty,Set.empty,Set.empty)) {
      case ((ind,send,ps),p) =>
        val NextStateProcess(i,s,ns) = p.tick
        val a = i.map(IndicationFrom(p.id,_))
        (ind ++ a,send ++ s,ps + ns)
    }
    NextStateScheduler(fs, fi,copy(processes = fp.map(p => p.id -> p).toMap, network = network.send(fs), step = step + 1))
  }

  // TODO aca deberia tener algun control de q digo deliver a 0, el paquete 1 -> 0
  def deliver(ds: DeliverBatch[M#Payload]) = {
    def deliver(d: Deliver) = processes(d.packet.to).deliver(d)
    val d = ds.ops.values.collect { case Left(value) => value }.toSet
    val drops = ds.ops.values.collect { case Right(value) => value }

    val nn = network.deliver(d).flatMap(_.drop(drops.map(_.packet).toSet)).get

    val (fi,fs,fp) = d.foldLeft[(Set[IndicationFrom[M#Ind]],Set[Send],Set[P])]((Set.empty,Set.empty,Set.empty)) {
      case ((ind,send,ps),d) =>
        val NextStateProcess(i,s,ns) = deliver(d)
        val a = i.map(IndicationFrom(d.packet.to,_))
        (ind ++ a,send ++ s,ps + ns)
    }

    NextStateScheduler(fs, fi,copy(processes = processes ++ fp.map(p => p.id -> p).toMap, network = nn.send(fs)))
  }

}
