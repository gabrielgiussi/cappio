package oss.giussi.cappio

import org.scalatest.{Matchers, WordSpec}

trait CappIOSpec extends WordSpec with Matchers {

  implicit class EnhancedNextState[M <: Mod](ns: NextState[M]){
    def tick: NextState[M] = ns.module.tick

    def deliver(p: Packet): NextState[M] = ns.module.tail.deliver(FLLDeliver(p))

    def request(req: M#Req): NextState[M] = ns.module.request(req)
  }
}
