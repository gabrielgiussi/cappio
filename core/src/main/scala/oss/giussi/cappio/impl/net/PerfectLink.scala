package oss.giussi.cappio.impl.net
import oss.giussi.cappio.Messages.ProcessLocal
import oss.giussi.cappio.impl.net.StubbornLink.{SLDeliver, SLSend}
import oss.giussi.cappio.{AbstractModule, ModS, Module, Packet, StateWithModule}

object PerfectLink {

  case class PLSend[P](packet: Packet[P])

  case class PLDeliver[P](packet: Packet[P])

  type PLModule[P] = ModS[StubLink[P]] {
    type Req = PLSend[P]
    type Ind = PLDeliver[P]
    type S = PerfectLinkState[P]
  }

  sealed trait PerfectLinkState[P]

  case class PLDumbState[P](delivered: Set[Packet[P]], module: Module[StubLink[P]]) extends PerfectLinkState[P] with StateWithModule[StubLink[P],PLDumbState[P]] {
    def alreadyDelivered(d: Packet[P]): Boolean = delivered contains d

    def deliver(p: Packet[P]) = copy(delivered + p)

    override def updateModule(m: Module[StubLink[P]]): PLDumbState[P] = copy(module = m)
  }

  /*
  case class PLSmartState[P](state: PerfectLinkState[P], module: Module[StubLink[P]]) extends StateWithModule[StubLink[P], PLSmartState[P]] {
    override def updateModule(m: Module[StubLink[P]]) = copy(module = m)

    def deliver(p: Packet[P]) = copy(state = state.deliver(p))
  }

   */

  def processLocal[P]: ProcessLocal[PLSend[P], PerfectLinkState[P], PLDeliver[P], SLSend[P], SLDeliver[P], P] = {
    import oss.giussi.cappio.Messages._
    (msg, state) =>
      msg match {
        case PublicRequest(PLSend(p)) => LocalStep.withRequests(Set(LocalRequest(SLSend(p))), state)
          // FIXME asInstanceOf
        case LocalIndication(SLDeliver(p)) if !state.asInstanceOf[PLDumbState[P]].alreadyDelivered(p) => LocalStep.withIndications(Set(PLDeliver(p)), state.asInstanceOf[PLDumbState[P]].deliver(p))
        case _ => LocalStep.withState(state)
      }
  }
    def init[P] = apply[P] _

  def apply[P](timeout: Int): Module[PLModule[P]] = AbstractModule.mod[PLModule[P],PLModule[P]#Dep](PLDumbState(Set.empty[Packet[P]],StubbornLink.init(timeout)),processLocal[P])

  def smart[P](timeout: Int): Module[PLModule[P]] = AbstractModule.mod[PLModule[P],PLModule[P]#Dep](???,???)

}
