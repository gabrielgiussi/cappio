package oss.giussi.cappio

import org.scalatest.{Matchers, WordSpec}

trait CappIOSpec extends WordSpec with Matchers {

  implicit class EnhancedNextState[Req,State,Ind](ns: NextState[Req,State,Ind]){
    def tick: NextState[Req,State,Ind] = ns.module.tick

    def deliver(p: Packet): NextState[Req,State,Ind] = ns.module.tail.deliver(FLLDeliver(p))

    def request(req: Req): NextState[Req,State,Ind] = ns.module.request(req)
  }
}
