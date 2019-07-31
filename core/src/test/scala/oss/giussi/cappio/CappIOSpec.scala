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

    def deliver(ps: (ProcessId, Packet[M#Payload])*) = sch.deliver(DeliverBatch(ps.map { case (id, p) => id -> Left(FLLDeliver(p)) }.toMap))

    def req(ps: (ProcessId, M#Req)*) = sch.request(RequestBatch(ps.toMap))

  }

  implicit class EnhancedWaitingDeliver[M <: Mod](ns: (Set[FLLSend[M#Payload]], Set[IndicationFrom[M#Ind]], WaitingDeliver[M])) {

    def deliver(ps: (ProcessId, Packet[M#Payload])*) = ns._3.deliver(DeliverBatch(ps.map { case (id, p) => id -> Left(FLLDeliver(p)) }.toMap)) // TODO duplicated

  }

  implicit class EnhancedWaitingRequest[M <: Mod](ns: (Set[FLLSend[M#Payload]], Set[IndicationFrom[M#Ind]], WaitingRequest[M])) {

    def req(ps: (ProcessId, M#Req)*) = ns._3.request(RequestBatch(ps.toMap))

  }

}

trait SchedulerSupport[M <: Mod] {


  sealed trait Input

  case class Req(p: ProcessId, r: M#Req) extends Input

  case class Del(p: PacketId[M#Payload]) extends Input

  case class Drop(p: PacketId[M#Payload]) extends Input

  case class Crash(p: ProcessId) extends Input

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
          val NextStateTickScheduler(_, ind, nsch) = scheduler.request(RequestBatch(p -> r))
          sch(nsch, tail, indications ++ ind)
        case Del(id) :: tail =>
          val inTransit = findPacket(id)
          val NextStateTickScheduler(_, ind, nsch) = scheduler.deliver(DeliverBatch(Left(inTransit.deliver)))
          sch(nsch, tail, indications ++ ind)
        case Drop(id) :: tail =>
          val inTransit = findPacket(id)
          val NextStateTickScheduler(_, ind, nsch) = scheduler.deliver(DeliverBatch(Right(inTransit.drop)))
          sch(nsch, tail, indications ++ ind)
        case JustTick :: tail =>
          val NextStateTickScheduler(_, ind, nsch) = scheduler.request(RequestBatch.empty)
          sch(nsch, tail, indications ++ ind)
      }
    }

    sch(scheduler, ops, List.empty)
  }
}
