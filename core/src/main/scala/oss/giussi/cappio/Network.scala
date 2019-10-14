package oss.giussi.cappio

import java.util.UUID

import oss.giussi.cappio.Network.InTransitPacket
import oss.giussi.cappio.impl.net.FairLossLink.FLLSend

import scala.util.{Failure, Success, Try}

object Payload {
  def apply[T](msg: T): Payload[T] = new Payload(UUID.randomUUID(), msg)
}

case class Payload[T](id: UUID, msg: T)

object Packet {
  @deprecated
  def apply[T](from: Int, to: Int, payload: T, instance: Instance): Packet[T] = new Packet(UUID.randomUUID(), payload, ProcessId(from), ProcessId(to), instance)

  def apply[T](from: ProcessId, to: ProcessId, payload: T, instance: Instance): Packet[T] = new Packet(UUID.randomUUID(), payload, from, to, instance)

  def apply[T](payload: Payload[T], from: ProcessId, to: ProcessId, instance: Instance): Packet[T] = new Packet(payload.id, payload.msg, from, to, instance)
}

// Deberian poder ser creados solo por las abstraction de network
case class Packet[T](id: UUID, payload: T, from: ProcessId, to: ProcessId, instance: Instance)

case class Drop[T](packet: Packet[T]) // TODO drop may be just the uuid

object Network {

  sealed trait InTransitPacket[T] {

    val packet: Packet[T]

    def deliver: FLLDeliver[T]

    def drop: Drop[T]
  }

  def init[T] = Network(Set.empty[Packet[T]],Set.empty[Packet[T]])

}
/*
 TODO increase step on tick or send/deliver?
  - on send/deliver  => no me permite hacer un send/deliver en el mismo step.
 */

object FLLDeliver {

  // TODO i want to hide this constructor to assure that only the network can give me FLLDeliver's via inTransit method
  // this has two main issues
  // - makes more difficult to test abstractions, unless I test processLocal instead of the whole process! (then I don't have to send FLLDeliver, just indications from the dependency)
  // - I have one problem in the CombinedModule when I must create FLLDeliver with the correspondent type, aka Inl(payload) to just payload.
  // I can use the withPayload method but then I could change the payload to anything! i just want to unwrap the payload, I require some method unwrap for the FLLDeliver when the payload
  // is a Coproduct
  def apply[P](packet: Packet[P]): FLLDeliver[P] = new FLLDeliver(packet){}
}

sealed abstract case class FLLDeliver[P](packet: Packet[P]) {
  // TODO not used
  def withPayload[NP](newPayload: NP) = new FLLDeliver(packet.copy(payload = newPayload)) {}
}

// TODO I added alreadyDelivered only to be able to check the "liveness" condition (see BEBLevel broken) but maybe I shlould save this info somewhere else.
case class Network[T](packets: Set[Packet[T]], alreadyDelivered: Set[Packet[T]]) {


  def drop(drops: Set[Drop[T]]): Try[Network[T]] = {
    val p = drops.map(_.packet)
    if (p.forall(packets.contains)) Success(copy(packets -- p)) else Failure(new RuntimeException("Some packets are not in transit"))
  }

  def deliver(delivered: Set[FLLDeliver[T]]): Try[Network[T]] = {
    val p = delivered.map(_.packet)
    if (p.forall(packets.contains))
      Success(copy(packets -- p, alreadyDelivered ++ p))
    else Failure(new RuntimeException)
  }

  def send(sent: Set[FLLSend[T]]): Network[T] = copy(packets ++ sent.map(_.packet))

  def inTransit: Set[InTransitPacket[T]] = packets.map(p => new InTransitPacket[T] {
    val packet = p

    override def deliver: FLLDeliver[T] = new FLLDeliver(packet) {}

    override def drop: Drop[T] = Drop(p)
  })


}
