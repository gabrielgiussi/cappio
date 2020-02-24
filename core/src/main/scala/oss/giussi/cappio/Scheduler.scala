package oss.giussi.cappio

import oss.giussi.cappio.Network.InTransitPacket
import oss.giussi.cappio.impl.net.FairLossLink.FLLSend

object Scheduler {

  type AutoDeliver[P] = Packet[P] => Boolean

  // TODO Why I can't say AutoDeliver[_] here
  def selfDelivery[P]: AutoDeliver[P] = packet => packet.from == packet.to

  def init[M <: Mod](processes: Set[Process[M]]): Scheduler[M] = Scheduler(processes.map(p => p.id -> p).toMap, Network.init[M#Payload], 0, selfDelivery)

  def init[M <: Mod](processes: Set[ProcessId], f: ProcessId => Module[M]): Scheduler[M] = Scheduler(processes.map(pId => pId -> Process(pId, f(pId))).toMap, Network.init[M#Payload], 0, selfDelivery)

  def withAutoDeliver[M <: Mod](processes: Set[ProcessId], f: ProcessId => Module[M], autoDeliver: AutoDeliver[M#Payload]): Scheduler[M] = {
    val ad: AutoDeliver[M#Payload] = packet => selfDelivery(packet) || autoDeliver(packet)
    Scheduler(processes.map(pId => pId -> Process(pId, f(pId))).toMap, Network.init[M#Payload], 0, ad)
  }

  // TODO deprecated
  def requestAndTick[M <: Mod](scheduler: Scheduler[M]) = scheduler.request _ andThen { case NextStateScheduler(sent0, ind0, sch0) =>
    val NextStateScheduler(sent1, ind1, sch1) = sch0.tick
    NextStateScheduler(sent0 ++ sent1, ind0 ++ ind1, sch1)
  }

  // TODO deprecated
  def deliverAndTick[M <: Mod](scheduler: Scheduler[M]) = scheduler.deliver _ andThen { case NextStateScheduler(sent0, ind0, sch0) =>
    val NextStateScheduler(sent1, ind1, sch1) = sch0.tick
    NextStateScheduler(sent0 ++ sent1, ind0 ++ ind1, sch1)
  }
}

sealed trait ProcessInput[+R] {
  val predefined: Boolean
}

object ProcessRequest {

  def predefined[R](id: ProcessId, request: R) = ProcessRequest(id,request,true)

}

case class ProcessRequest[R](id: ProcessId, request: R, predefined: Boolean = false) extends ProcessInput[R]

case class Crash(id: ProcessId, predefined: Boolean = false) extends ProcessInput[Nothing]

object DeliverBatch {

  def empty[P]: DeliverBatch[P] = new DeliverBatch(Map.empty)

  def apply[P](ops: Either[FLLDeliver[P], Drop[P]]*): DeliverBatch[P] = new DeliverBatch(ops.map {
    case d@Left(FLLDeliver(p)) => p.to -> d
    case d@Right(Drop(p)) => p.to -> d
  }.toMap)
}

// deliver shoulnd't receive a map, just a list of delivers (the packet contains the target process)
case class DeliverBatch[P](ops: Map[ProcessId, Either[FLLDeliver[P], Drop[P]]]) {
  /*
  def add(deliver: FLLDeliver[P]) = copy(ops = ops + (deliver.packet.to -> Left(deliver)))

  def add(drop: Drop[P]) = copy(ops = ops + (drop.packet.to -> Right(drop)))

  def remove(deliver: FLLDeliver[P]) = copy(ops = ops - deliver.packet.to)

  def remove(drop: Drop[P]) = copy(ops = ops - drop.packet.to)

  def clear = DeliverBatch.empty

   */
}

sealed trait Step[M <: Mod] {
  val scheduler: Scheduler[M]
}

trait NextResult[M <: Mod] {
  def sent: Set[FLLSend[M#Payload]]
  def ind: Set[IndicationFrom[M#Ind]]
}

case class RequestResult[M <: Mod](sent: Set[FLLSend[M#Payload]], ind: Set[IndicationFrom[M#Ind]], waiting: WaitingDeliver[M]) extends NextResult[M] {
  def deliver = waiting.deliver
}

case class DeliverResult[M <: Mod](sent: Set[FLLSend[M#Payload]], ind: Set[IndicationFrom[M#Ind]], waiting: WaitingRequest[M]) extends NextResult[M] {
  def request = waiting.request
}

case class WaitingRequest[M <: Mod](scheduler: Scheduler[M]) extends Step[M] {
  def request = (scheduler.request _).andThen { case NextStateScheduler(sent, ind, sch) =>
    RequestResult(sent, ind, WaitingDeliver(sch))
  }
}

case class WaitingDeliver[M <: Mod](scheduler: Scheduler[M]) extends Step[M] {
  def deliver = (scheduler.deliver _).andThen { case NextStateScheduler(sent, ind, sch) =>
    RequestResult(sent,ind,WaitingDeliver(sch))
  }

  def tick = {
    val NextStateScheduler(sent,indications,sch) = scheduler.tick
    DeliverResult(sent,indications,WaitingRequest(sch))
  }
}

case class NextStateScheduler[M <: Mod](sent: Set[FLLSend[M#Payload]], indications: Set[IndicationFrom[M#Ind]], scheduler: Scheduler[M])

case class IndicationFrom[I](p: ProcessId, i: I)

case class Scheduler[M <: Mod](processes: Map[ProcessId, Process[M]], network: Network[M#Payload], step: Int, autodeliver: Packet[M#Payload] => Boolean) {

  type Self = Scheduler[M]

  type P = Process[M]

  type Next = NextStateScheduler[M]

  type Send = FLLSend[M#Payload]

  type Deliver = FLLDeliver[M#Payload]

  def isDown(p: ProcessId): Option[Boolean] = processes.get(p).map(_.status == Down)

  //TODO Avoid send a crash and a request to the same process id
  def request(requests: ProcessInput[M#Req]*) = {
    val (batch, crashes) = requests.foldLeft((Map.empty[ProcessId, M#Req], Set.empty[ProcessId])) {
      case ((r, c), ProcessRequest(id, a, _)) => (r updated(id, a), c)
      case ((r, c), Crash(id,_)) => (r, c + id)
    }

    val crashed = crashes.foldLeft(Set.empty[Process[M]]) { case (acc, id) => processes.get(id) match {
      case None => acc
      case Some(p) => acc + p.crash
    }
    }

    val filteredBatch = batch.filterNot(p => isDown(p._1).get) // Doesn't consider new crashed process but should check before if I've received a Crash and a Request for the same process

    val (fi, fs, fp) = filteredBatch.foldLeft[(Set[IndicationFrom[M#Ind]], Set[Send], Set[P])]((Set.empty, Set.empty, Set.empty)) {
      case ((ind, send, ps), (pid, req)) =>
        val NextStateProcess(i, s, ns) = processes(pid).request(req)
        val a = i.map(IndicationFrom(pid, _))
        (ind ++ a, send ++ s, ps + ns)
    }

    autodelivery(NextStateScheduler(fs, fi, copy(processes = processes ++ (crashed ++ fp).map(p => p.id -> p), network = network.send(fs))))
  }
  private def autodelivery(ns: Next): Next = {
    val NextStateScheduler(sent0,ind0,sch0) = ns

    val maybe = ns.scheduler.network.inTransit.map(_.packet) ++ sent0.map(_.packet).filter(_.toSelf) // TODO patch I should only be able to autodeliver packets in the same step if they are toSelf.
    val autodeliveries = maybe.filter(autodeliver)
      .map(p => DeliverBatch(Map(p.to -> Left(FLLDeliver(p)))))

    val (sch1,sent1,ind1) = autodeliveries.foldLeft[(Scheduler[M],Set[FLLSend[M#Payload]],Set[IndicationFrom[M#Ind]])]((sch0,Set.empty,Set.empty)){ case ((sch,accSent,accInd),batch) =>
      val NextStateScheduler(nextSend,nextInd,nextSch) = sch deliver batch
      (nextSch,accSent ++ nextSend, accInd ++ nextInd)
    }
    NextStateScheduler(sent0 ++ sent1,ind0 ++ ind1,sch1)
  }

  def tick: Next = {
    val (fi, fs, fp) = processes.filter(_._2.status == Up).values.foldLeft[(Set[IndicationFrom[M#Ind]], Set[Send], Set[P])]((Set.empty, Set.empty, Set.empty)) {
      case ((ind, send, ps), p) =>
        val NextStateProcess(i, s, ns) = p.tick
        val a = i.map(IndicationFrom(p.id, _))
        (ind ++ a, send ++ s, ps + ns)
    }
    autodelivery(NextStateScheduler(fs, fi, copy(processes = fp.map(p => p.id -> p).toMap ++ processes.filter(_._2.status == Down), network = network.tick.send(fs), step = step + 1)))
  }

  // TODO aca deberia tener algun control de q digo deliver a 0, el paquete 1 -> 0
  def deliver(ds: DeliverBatch[M#Payload]) = {
    def deliver(d: Deliver) = {
      processes(d.packet.to) match {
        case p@Process(_, _, Up) => p deliver d
        case p => NextStateProcess[M](Set.empty,Set.empty,p) // TODO patch
      }
    }

    val d = ds.ops.values.collect { case Left(value) => value }.toSet
    val drops = ds.ops.values.collect { case Right(value) => value }

    val nn = network.deliver(d).flatMap(_.drop(drops.toSet)).get

    val (fi, fs, fp) = d.foldLeft[(Set[IndicationFrom[M#Ind]], Set[Send], Set[P])]((Set.empty, Set.empty, Set.empty)) {
      case ((ind, send, ps), d) =>
        val NextStateProcess(i, s, ns) = deliver(d)
        val a = i.map(IndicationFrom(d.packet.to, _))
        (ind ++ a, send ++ s, ps + ns)
    }

    NextStateScheduler(fs, fi, copy(processes = processes ++ fp.map(p => p.id -> p).toMap, network = nn.send(fs)))
  }

  def availableProcesses: Set[ProcessId] = processes.filter(_._2.status == Up).keySet

  def availablePackets(showDelivered: Boolean): Set[InTransitPacket[M#Payload]] = network
    .inTransit
    .filterNot(p => isDown(p.packet.to).get)
    .filter(p => showDelivered || !network.alreadyDelivered.contains(p.packet))

}
