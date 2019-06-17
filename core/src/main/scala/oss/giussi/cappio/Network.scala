package oss.giussi.cappio

import java.util.UUID

import oss.giussi.cappio.Network.InTransitPacket
import oss.giussi.cappio.impl.bcast.UniformReliableBroadcast.Payload
import oss.giussi.cappio.impl.net.FairLossLink.FLLSend

import scala.util.{Failure, Success, Try}

object Packet {
  def apply(from: Int, to: Int, payload: Any, instance: Instance): Packet = new Packet(UUID.randomUUID(), payload, ProcessId(from), ProcessId(to), instance)

  def apply(from: ProcessId, to: ProcessId, payload: Any, instance: Instance): Packet = new Packet(UUID.randomUUID(), payload, from, to, instance)

  def apply(payload: Payload, from: ProcessId, to: ProcessId, instance: Instance): Packet = new Packet(payload.id, payload.msg, from, to, instance)
}

// Deberian poder ser creados solo por las abstraction de network
case class Packet(id: UUID, payload: Any, from: ProcessId, to: ProcessId, instance: Instance)

case class Drop(packet: Packet)

object Network {

  //sealed trait InTransitPacket { TODO
  trait InTransitPacket {

    val packet: Packet

    def deliver: FLLDeliver

    def drop: Drop
  }

  def init() = Network(Set.empty)

}
/*
 TODO increase step on tick or send/deliver?
  - on send/deliver  => no me permite hacer un send/deliver en el mismo step.
 */

case class FLLDeliver(packet: Packet)

case class Network(packets: Set[Packet]) {

  private def remove(delivered: Set[Packet]): Network = copy(packets -- delivered)

  def drop(p: Set[Packet]): Try[Network] = if (p.forall(packets.contains)) Success(remove(p)) else Failure(new RuntimeException("FAILLLL"))

  def deliver(delivered: Set[FLLDeliver]): Try[Network] = {
    val p = delivered.map(_.packet)
    if (p.forall(packets.contains))
      Success(remove(p))
    else Failure(new RuntimeException)
  }

  def send(sent: Set[FLLSend]): Network = copy(packets ++ sent.map(_.packet))

  // packets are available, for drop or deliver. But how to limit FLLDeliver creation then? (should only be created from Network)
  // maybe InTransitPacket(p: Packet) with methods Drop => Drop(p) or Deliver => FLLDeliver(p)
  def available(): Set[FLLDeliver] = packets.map(FLLDeliver)

  def inTransit(): Set[InTransitPacket] = packets.map(p => new InTransitPacket {
    val packet = p

    override def deliver: FLLDeliver = FLLDeliver(p)

    override def drop: Drop = Drop(p)
  })


}
