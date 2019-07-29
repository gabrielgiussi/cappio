package oss.giussi.cappio

import org.scalatest.{Matchers, WordSpec}

trait CappIOSpec extends WordSpec with Matchers {

  implicit class P(i: Int) {
    def id = ProcessId(i)
  }

  implicit class EnhancedNextState[M <: Mod](ns: NextState[M]){
    def tick: NextState[M] = ns.module.tick

    def deliver(p: Packet[M#Payload]): NextState[M] = ns.module.tail.deliver(FLLDeliver(p))

    def request(req: M#Req): NextState[M] = ns.module.request(req)

    def packets: Set[Packet[M#Payload]] = ns.send.map(_.packet)
  }

}
