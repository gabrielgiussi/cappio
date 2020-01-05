package oss.giussi.cappio.impl.bcast

import oss.giussi.cappio.{CappIOSpec, Packet, Payload, ProcessId}
import shapeless.{Inl, Inr}

class UniformReliableBroadcastSpec extends CappIOSpec {

  val all = (0 to 2).map(ProcessId).toSet
  val timeout = 3
  val urb = UniformReliableBroadcast[String](all, timeout)(p0)

  import UniformReliableBroadcast._

  implicit def intToProcessId(i: Int): ProcessId = ProcessId(i)

  "UniformReliableBroadcast" should {
    "B" in {
      val payload = Payload("gaga")
      val uuid = payload.id
      urb.request(URBBcast(payload))
        .send.map(_.packet).filter(_.payload == Inr(Inl(URBData(ProcessId(0), "gaga")))).map { packet =>
        val Packet(`uuid`, _, ProcessId(0), to, _) = packet
        to.id
      } should contain theSameElementsAs Set(0, 1, 2)
    }
    "C" in {
      val payload = Payload("gaga")
      val urbPayload: Payload[URBMod[String]#Payload] = Payload(payload.id, Inr(Inl(URBData(0, payload.msg))))
      urb.request(URBBcast(payload))
        .deliver(Packet(urbPayload, 1.id, 0.id, BEB))
        .deliver(Packet(urbPayload, 2, 0, BEB))
        .deliver(Packet(urbPayload, 0, 0, BEB))
        .tick
        .indications should contain theSameElementsAs List(URBDeliver(0, payload.msg))
    }
  }

}
