package oss.giussi.cappio.impl.net

import oss.giussi.cappio.Messages.ProcessLocal
import oss.giussi.cappio.impl.net.PerfectLink.{PLDeliver, PLSend, PerfectLinkState}
import oss.giussi.cappio.impl.net.PerfectLinkBeta.{PLModule, PerfectLinkBetaState}
import oss.giussi.cappio.impl.net.StubbornLink.{SLDeliver, SLSend}
import oss.giussi.cappio._

object PerfectLinkBeta {

  type PLModule = ModS[StubLink] {
    type Req = PLSend
    type Ind = PLDeliver
    type S = PerfectLinkBetaState
  }

  case class PerfectLinkBetaState(state: PerfectLinkState, module: Module[StubLink]) extends StateWithModule[StubLink, PerfectLinkBetaState] {
    override def updateModule(m: Module[StubLink]): PerfectLinkBetaState = copy(module = m)

    def deliver(p: Packet) = copy(state = state.deliver(p))
  }

  def init(timeout: Int): PerfectLinkBeta = PerfectLinkBeta(PerfectLinkBetaState(PerfectLinkState(Set.empty), StubbornLink.init(timeout)))


  def processLocal(): ProcessLocal[PLSend, PerfectLinkBetaState, PLDeliver, SLSend, SLDeliver] = {
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

case class PerfectLinkBeta(state: PerfectLinkBetaState) extends AbstractModule[PLModule,StubLink] {

  //override type S = PerfectLinkBetaState

  override def copyModule(ns: PerfectLinkBetaState): AbstractModule[PLModule,StubLink] = copy(state = ns)

  override val processLocal: PLocal = PerfectLinkBeta.processLocal()
}
