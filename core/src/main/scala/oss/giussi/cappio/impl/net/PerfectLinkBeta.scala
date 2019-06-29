package oss.giussi.cappio.impl.net

import oss.giussi.cappio.Messages.ProcessLocal
import oss.giussi.cappio.impl.net.PerfectLink.{PLDeliver, PLSend, PerfectLinkState}
import oss.giussi.cappio.impl.net.PerfectLinkBeta.PerfectLinkBetaState
import oss.giussi.cappio.impl.net.StubbornLink.{SLDeliver, SLSend, StubbornLinkState}
import oss.giussi.cappio.{AbstractModule, Module, Packet, StateWithModule}

object PerfectLinkBeta {

  case class PerfectLinkBetaState(state: PerfectLinkState, module: Module[SLSend, StubbornLinkState, SLDeliver]) extends StateWithModule[SLSend, StubbornLinkState, SLDeliver, PerfectLinkBetaState] {
    override def updateModule(m: Module[SLSend, StubbornLinkState, SLDeliver]): PerfectLinkBetaState = copy(module = m)

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

case class PerfectLinkBeta(state: PerfectLinkBetaState) extends AbstractModule[PLSend, PerfectLinkBetaState, PLDeliver, SLSend, StubbornLinkState, SLDeliver] {

  import oss.giussi.cappio.Messages._

  override def copyModule(ns: PerfectLinkBetaState): AbstractModule[PLSend, PerfectLinkBetaState, PLDeliver, SLSend, StubbornLinkState, SLDeliver] = copy(state = ns)

  def processLocal(l: LMsg, state: PerfectLinkBetaState): LStep = l match {
    case PublicRequest(PLSend(p)) => LocalStep.withRequests(Set(LocalRequest(SLSend(p))), state) // state.module.request(SLSend(p)) // This could also be a LocalStep
    case Tick => LocalStep.withState(state)
    case LocalIndication(SLDeliver(p)) if !state.state.alreadyDelivered(p) => LocalStep.withIndications(Set(PLDeliver(p)), state.deliver(p))
    case LocalIndication(SLDeliver(p)) => LocalStep.withState(state)
  }

  override val processLocal: PLocal = PerfectLinkBeta.processLocal()
}
