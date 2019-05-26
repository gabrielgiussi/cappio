package oss.giussi.cappio.impl.net

import oss.giussi.cappio.{AbstractModule, Module, NextState, Packet, StateWithModule}
import oss.giussi.cappio.impl.net.PerfectLink.{PLDeliver, PLSend, PerfectLinkState}
import oss.giussi.cappio.impl.net.PerfectLinkBeta.PerfectLinkBetaState
import oss.giussi.cappio.impl.net.StubbornLink.{SLDeliver, SLSend, StubbornLinkState}

object PerfectLinkBeta {

  case class PerfectLinkBetaState(state: PerfectLinkState, module: Module[SLSend, StubbornLinkState, SLDeliver]) extends StateWithModule[SLSend,StubbornLinkState,SLDeliver, PerfectLinkBetaState] {
    override def updateModule(m: Module[SLSend, StubbornLinkState, SLDeliver]): PerfectLinkBetaState = copy(module = m)

    def deliver(p: Packet) = copy(state = state.deliver(p))
  }

  def init(timeout: Int): PerfectLinkBeta = PerfectLinkBeta(PerfectLinkBetaState(PerfectLinkState(Set.empty),StubbornLink.init(timeout)))
}

case class PerfectLinkBeta(state: PerfectLinkBetaState) extends AbstractModule[PLSend,PerfectLinkBetaState,PLDeliver,SLSend,StubbornLinkState,SLDeliver] {
  override def copyModule(ns: PerfectLinkBetaState): AbstractModule[PLSend,PerfectLinkBetaState, PLDeliver, SLSend, StubbornLinkState, SLDeliver] = copy(state = ns)

  override def processLocal(l: LocalMsg, state: PerfectLinkBetaState): LocalStep = l match {
    case PublicRequest(PLSend(p)) => LocalStep.localRequest(Set(LocalRequest(SLSend(p))),state)  // state.module.request(SLSend(p)) // This could also be a LocalStep
    case Tick => LocalStep(state)
    case LocalIndication(SLDeliver(p)) if !state.state.alreadyDelivered(p) => LocalStep(Set(PLDeliver(p)),state.deliver(p))
    case LocalIndication(SLDeliver(p)) => LocalStep(state)
  }

}
