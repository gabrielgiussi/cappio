package oss.giussi.cappio

import java.util.UUID

import oss.giussi.cappio.Network.InTransitPacket
import oss.giussi.cappio.impl.net.FairLossLink.FLLSend

import scala.util.{Failure, Success, Try}

object Payload {
  def apply[T](msg: T): Payload[T] = new Payload(UUID.randomUUID(), msg)
}

// TODO porque necesitaba poner el id aca en lugar de en el Packet?

case class Payload[T](id: UUID, msg: T) // Payload[T](id: UUID, msg: T) ??

object Packet {
  def apply[T](from: Int, to: Int, payload: T, instance: Instance): Packet[T] = new Packet(UUID.randomUUID(), payload, ProcessId(from), ProcessId(to), instance)

  def apply[T](from: ProcessId, to: ProcessId, payload: T, instance: Instance): Packet[T] = new Packet(UUID.randomUUID(), payload, from, to, instance)

  def apply[T](payload: Payload[T], from: ProcessId, to: ProcessId, instance: Instance): Packet[T] = new Packet(payload.id, payload.msg, from, to, instance)
}

// Deberian poder ser creados solo por las abstraction de network
case class Packet[T](id: UUID, payload: T, from: ProcessId, to: ProcessId, instance: Instance)

case class Drop[T](packet: Packet[T])

object Network {

  //sealed trait InTransitPacket { TODO
  trait InTransitPacket[T] {

    val packet: Packet[T]

    def deliver: FLLDeliver[T]

    def drop: Drop[T]
  }

  def init[T]() = Network(Set.empty[Packet[T]])

}
/*
 TODO increase step on tick or send/deliver?
  - on send/deliver  => no me permite hacer un send/deliver en el mismo step.
 */

case class FLLDeliver[T](packet: Packet[T])

case class Network[T](packets: Set[Packet[T]]) {

  private def remove(delivered: Set[Packet[T]]): Network[T] = copy(packets -- delivered)

  def drop(p: Set[Packet[T]]): Try[Network[T]] = if (p.forall(packets.contains)) Success(remove(p)) else Failure(new RuntimeException("FAILLLL"))

  def deliver(delivered: Set[FLLDeliver[T]]): Try[Network[T]] = {
    val p = delivered.map(_.packet)
    if (p.forall(packets.contains))
      Success(remove(p))
    else Failure(new RuntimeException)
  }

  def send(sent: Set[FLLSend[T]]): Network[T] = copy(packets ++ sent.map(_.packet))

  // packets are available, for drop or deliver. But how to limit FLLDeliver creation then? (should only be created from Network)
  // maybe InTransitPacket(p: Packet) with methods Drop => Drop(p) or Deliver => FLLDeliver(p)
  def available(): Set[FLLDeliver[T]] = packets.map(FLLDeliver.apply[T])

  def inTransit(): Set[InTransitPacket[T]] = packets.map(p => new InTransitPacket[T] {
    val packet = p

    override def deliver: FLLDeliver[T] = FLLDeliver(p)

    override def drop: Drop[T] = Drop(p)
  })


}
