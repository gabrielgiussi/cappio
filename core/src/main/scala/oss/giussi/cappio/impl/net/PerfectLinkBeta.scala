package oss.giussi.cappio.impl.net

import oss.giussi.cappio.Messages.ProcessLocal
import oss.giussi.cappio.impl.net.PerfectLink.{PLDeliver, PLSend, PerfectLinkState}
import oss.giussi.cappio.impl.net.PerfectLinkBeta.{PLModule, PerfectLinkBetaState}
import oss.giussi.cappio.impl.net.StubbornLink.{SLDeliver, SLSend}
import oss.giussi.cappio._

object PerfectLinkBeta {

  type PLModule[P] = ModS[StubLink[P],P] {
    type Req = PLSend[P]
    type Ind = PLDeliver[P]
    type S = PerfectLinkBetaState[P]
  }

  case class PerfectLinkBetaState[P](state: PerfectLinkState[P], module: Module[StubLink[P]]) extends StateWithModule[StubLink[P], PerfectLinkBetaState[P]] {
    override def updateModule(m: Module[StubLink[P]]) = copy(module = m)

    def deliver(p: Packet[P]) = copy(state = state.deliver(p))
  }

  def init[P](timeout: Int): PerfectLinkBeta[P] = PerfectLinkBeta(PerfectLinkBetaState(PerfectLinkState(Set.empty), StubbornLink.init(timeout)))


  def processLocal[P]: ProcessLocal[PLSend[P], PerfectLinkBetaState[P], PLDeliver[P], SLSend[P], SLDeliver[P], P] = {
    import oss.giussi.cappio.Messages._
    (msg, state) =>
      msg match {
        case PublicRequest(PLSend(p)) => LocalStep.withRequests(Set(LocalRequest(SLSend(p))), state) // state.module.request(SLSend(p)) // This could also be a LocalStep
        case Tick => LocalStep.withState(state)
        case LocalIndication(SLDeliver(p)) if !state.state.alreadyDelivered(p) => LocalStep.withIndications(Set(PLDeliver(p)), state.deliver(p))
        case LocalIndication(SLDeliver(p)) => LocalStep.withState(state)
      }
  }
}

case class PerfectLinkBeta[P](state: PerfectLinkBetaState[P]) extends AbstractModule[PLModule[P],StubLink[P],P] {

  //override type S = PerfectLinkBetaState

  override def copyModule(ns: PerfectLinkBetaState[P]) = copy(state = ns)

  override val processLocal: PLocal = PerfectLinkBeta.processLocal
}
