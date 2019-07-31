package oss.giussi.cappio

import org.scalatest.{Matchers, WordSpec}
import oss.giussi.cappio.impl.net.FairLossLink.FLLSend
import oss.giussi.cappio.impl.net.StubbornLink.{SLDeliver, SLSend}

trait CappIOSpec extends WordSpec with Matchers {

  val _ALL = (0 to 5).map(ProcessId)

  val ALL = _ALL.toSet

  val List(p0,p1,p2,p3,p4,p5) = _ALL.toList

  val _0_to_2 = ALL.take(3)

  case class SimplePacket[T](from: ProcessId, to: ProcessId, payload: T)

  def $p[T](from: ProcessId, to: ProcessId, payload: T) = SimplePacket(from,to,payload)

  implicit class P(i: Int) {
    def id = ProcessId(i)
  }

  implicit class EnhancedPacket[P](p: Packet[P]) {
    def slSend = SLSend(p)

    def slDeliver = SLDeliver(p)
  }

  implicit class EnhancedNextState[M <: Mod](ns: NextState[M]){
    def tick: NextState[M] = ns.module.tick

    def deliver(p: Packet[M#Payload]): NextState[M] = ns.module.tail.deliver(FLLDeliver(p))

    def request(req: M#Req): NextState[M] = ns.module.request(req)

    def packets: Set[Packet[M#Payload]] = ns.send.map(_.packet)

    def simplePackets = packets.map(p => SimplePacket(p.from,p.to,p.payload))
  }

  implicit class EnhancedNextStateScheduler[M <: Mod](ns: NextStateScheduler[M]) extends EnhancedScheduler(ns.scheduler)

  implicit class EnhancedScheduler[M <: Mod](sch: Scheduler[M]){

    def deliver(ps: (ProcessId,Packet[M#Payload])*)  = sch.deliver(DeliverBatch(ps.map { case (id,p) => id -> Left(FLLDeliver(p)) }.toMap))

    def req(ps: (ProcessId,M#Req)*) = sch.request(RequestBatch(ps.toMap))

  }

  implicit class EnhancedWaitingDeliver[M <: Mod](ns: (Set[FLLSend[M#Payload]], Set[IndicationFrom[M#Ind]], WaitingDeliver[M])) {

    def deliver(ps: (ProcessId,Packet[M#Payload])*) = ns._3.deliver(DeliverBatch(ps.map { case (id,p) => id -> Left(FLLDeliver(p)) }.toMap)) // TODO duplicated

  }

  implicit class EnhancedWaitingRequest[M <: Mod](ns: (Set[FLLSend[M#Payload]], Set[IndicationFrom[M#Ind]], WaitingRequest[M])) {

    def req(ps: (ProcessId,M#Req)*) = ns._3.request(RequestBatch(ps.toMap))

  }

}
