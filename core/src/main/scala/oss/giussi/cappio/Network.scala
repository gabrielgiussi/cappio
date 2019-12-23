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
case class Packet[T](id: UUID, payload: T, from: ProcessId, to: ProcessId, instance: Instance) {
  def toSelf = from == to
}

case class Drop[T](packet: Packet[T]) // TODO drop may be just the uuid

object Network {

  sealed trait InTransitPacket[T] {

    val packet: Packet[T]

    def deliver: FLLDeliver[T]

    def drop: Drop[T]
  }

  def init[T] = Network(0, Set.empty[IndexedPacket[T]], Set.empty[IndexedPacket[T]],Set.empty[Packet[T]])

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

case class IndexedPacket[T](sent: Int, packet: Packet[T])

// TODO I added alreadyDelivered only to be able to check the "liveness" condition (see BEBLevel broken) but maybe I shlould save this info somewhere else.
case class Network[T](index: Int, allSent: Set[IndexedPacket[T]], available: Set[IndexedPacket[T]], alreadyDelivered: Set[Packet[T]]) {

  def drop(drops: Set[Drop[T]]): Try[Network[T]] = {
    val toDrop = drops.map(_.packet)
    if (toDrop.forall(available.map(_.packet).contains)) Success(copy(available = available.filterNot(p => toDrop.contains(p.packet)))) else Failure(new RuntimeException("Some packets are not in transit"))
  }

  def deliver(delivered: Set[FLLDeliver[T]]): Try[Network[T]] = {
    val toDeliver = delivered.map(_.packet)
    if (toDeliver.forall(available.map(_.packet).contains))
      Success(copy(available = (available.filterNot(p => toDeliver.contains(p.packet))), alreadyDelivered = (alreadyDelivered ++ toDeliver)))
    else Failure(new RuntimeException)
  }

  private def sendOne(packet: Packet[T]): Network[T] = {
    allSent.find(_.packet == packet) match {
      case Some(IndexedPacket(i, _)) => copy(available = available + IndexedPacket(i,packet))
      case None =>
        val indexedPacket = IndexedPacket(index,packet)
        copy(available = available + indexedPacket, allSent = allSent + indexedPacket)
    }
  }

  def send(sent: Set[FLLSend[T]]): Network[T] = sent.map(_.packet).foldLeft(this)(_ sendOne _)

  def tick = copy(index = index + 1)

  def inTransit: Set[InTransitPacket[T]] = available.filter(_.sent < index).map(p => new InTransitPacket[T] {
    val packet = p.packet

    override def deliver: FLLDeliver[T] = new FLLDeliver(packet) {}

    override def drop: Drop[T] = Drop(packet)
  })

  def badPackets(delay: Int) = allSent.filterNot(s => alreadyDelivered.contains(s.packet))
    .filter(s => (index - s.sent) > delay) // TODO this is ok????
    .map(_.packet)


}
