package oss.giussi.cappio

import org.scalatest.{Matchers, WordSpec}
import oss.giussi.cappio.impl.net.FairLossLink.FLLSend
import oss.giussi.cappio.impl.net.StubbornLink.{SLDeliver, SLSend}

import scala.annotation.tailrec

case class PacketId[T](from: ProcessId, to: ProcessId, payload: T)

trait CappIOSpec extends WordSpec with Matchers {

  val ALL: List[ProcessId] = (0 to 5).map(ProcessId).toList

  val List(p0, p1, p2, p3, p4, p5) = ALL

  def $p[T](from: ProcessId, to: ProcessId, payload: T) = PacketId(from, to, payload)

  implicit class P(i: Int) {
    def id = ProcessId(i)
  }

  implicit class EnhancedPacket[P](p: Packet[P]) {
    def slSend = SLSend(p)

    def slDeliver = SLDeliver(p)

    def asId: PacketId[P] = PacketId(p.from,p.to,p.payload)
  }

  implicit class EnhancedProcesId(id: ProcessId){
    def -->[R](req: R): Request[R] = Request(id, req)

    def -->>[P](to: ProcessId,payload: P): Packet[P] = Packet(id,to,payload,Instance.ANY)
  }

  implicit class EnhancedNextState[M <: Mod](ns: NextState[M]) {
    def tick: NextState[M] = ns.module.tick

    def deliver(p: Packet[M#Payload]): NextState[M] = ns.module.tail.deliver(FLLDeliver(p))

    def request(req: M#Req): NextState[M] = ns.module.request(req)

    def packets: Set[Packet[M#Payload]] = ns.send.map(_.packet)

    def simplePackets = packets.map(p => PacketId(p.from, p.to, p.payload))
  }

  implicit class EnhancedNextStateScheduler[M <: Mod](ns: NextStateScheduler[M]) extends EnhancedScheduler(ns.scheduler)

  implicit class EnhancedScheduler[M <: Mod](sch: Scheduler[M]) {

    private def findInTransit(packets: Set[PacketId[M#Payload]]) =  {
      val inTransit = sch.network.inTransit.filter(int => packets.contains(int.packet.asId))
      if (inTransit.size == packets.size) inTransit
      else throw new RuntimeException("Some required packets are not present in the network")
    }

    // TODO this should receive a PacketId, not a Packet
    def deliver(ps: PacketId[M#Payload]*) = {
      val delivers = findInTransit(ps.toSet).map(_.deliver).map(p => p.packet.to -> Left(p)).toMap
      sch.deliver(DeliverBatch(delivers))
    }

    def req(ps: (ProcessId, M#Req)*) = sch.request(ps.map { case (id,req) => Request(id,req) } : _*)

    // TODO def send(packet: Packet[M#Req]) = sch.request(Seq(Request(packet.from,SLSend(packet))))

    def crash(ids: ProcessId*) = sch.request(ids.map(Crash) : _*)

    def drop(packets: PacketId[M#Payload]*) = {
      /*
      val drops = sch.network.inTransit.map(_.drop).filter { case Drop(Packet(_,payload,from,to,_)) =>
        val id = PacketId(from,to,payload)
        packets.toSet.contains(id)
      }.map(Right.apply).toList
       */
      val drops = findInTransit(packets.toSet).map(_.drop).map(Right(_)).toList
      sch.deliver(DeliverBatch.apply(drops : _*))
    }

    def network = sch.network

    def processes = sch.processes

  }

  implicit class EnhancedWaitingDeliver[M <: Mod](ns: (Set[FLLSend[M#Payload]], Set[IndicationFrom[M#Ind]], WaitingDeliver[M])) {

    def deliver(ps: (ProcessId, Packet[M#Payload])*) = ns._3.deliver(DeliverBatch(ps.map { case (id, p) => id -> Left(FLLDeliver(p)) }.toMap)) // TODO duplicated

  }

  /*
  implicit class EnhancedWaitingRequest[M <: Mod](ns: (Set[FLLSend[M#Payload]], Set[IndicationFrom[M#Ind]], WaitingRequest[M])) {

    def req(ps: (ProcessId, M#Req)*) = ns._3.request(RequestBatch(ps.toMap))

  }

   */

}

trait SchedulerSupport[M <: Mod] {


  sealed trait Input // TODO rename

  case class Req(p: ProcessId, r: M#Req) extends Input

  case class Del(p: PacketId[M#Payload]) extends Input

  case class Drop(p: PacketId[M#Payload]) extends Input

  case class CrashTest(p: ProcessId) extends Input

  case object JustTick extends Input

  def req(p: ProcessId, r: M#Req): Input = Req(p, r)

  def deliver(p: PacketId[M#Payload]): Input = Del(p)

  def drop(p: PacketId[M#Payload]): Input = Drop(p)

  def tick: Input = JustTick

  def schedule(scheduler: TickScheduler[M], ops: List[Input]): List[IndicationFrom[M#Ind]] = {
    @tailrec
    def sch(scheduler: TickScheduler[M], ops: List[Input], indications: List[IndicationFrom[M#Ind]]): List[IndicationFrom[M#Ind]] = {
      def findPacket(id: PacketId[M#Payload]) = {
        scheduler.scheduler.network.inTransit.find(p => p.packet.from == id.from && p.packet.to == id.to && p.packet.payload == id.payload) match {
          case None => throw new RuntimeException(s"Packet from ${id.from} to ${id.to} with payload ${id.payload} is not in transit")
          case Some(e) => e
        }
      }

      ops match {
        case Nil => indications
        case Req(p, r) :: tail =>
          val NextStateTickScheduler(_, ind, nsch) = scheduler.request(Seq(Request(p, r)))
          sch(nsch, tail, indications ++ ind) // TODO duplicated code
        case Del(id) :: tail =>
          val inTransit = findPacket(id)
          val NextStateTickScheduler(_, ind, nsch) = scheduler.deliver(DeliverBatch(Left(inTransit.deliver)))
          sch(nsch, tail, indications ++ ind)
        case Drop(id) :: tail =>
          val inTransit = findPacket(id)
          val NextStateTickScheduler(_, ind, nsch) = scheduler.deliver(DeliverBatch(Right(inTransit.drop)))
          sch(nsch, tail, indications ++ ind)
        case JustTick :: tail =>
          val NextStateTickScheduler(_, ind, nsch) = scheduler.request(Seq.empty)
          sch(nsch, tail, indications ++ ind)
        case CrashTest(id) :: tail =>
          val NextStateTickScheduler(_, ind, nsch) = scheduler.request(Seq(Crash(id)))
          sch(nsch, tail, indications ++ ind)
      }
    }

    sch(scheduler, ops, List.empty)
  }
}
