package oss.giussi.cappio.impl.net
import oss.giussi.cappio.Messages.ProcessLocal
import oss.giussi.cappio.impl.net.StubbornLink.{SLDeliver, SLSend}
import oss.giussi.cappio.{AbstractModule, Instance, ModS, Module, Packet, ProcessId, StateWithModule}

object PerfectLink {

  object PLSend {
    def external[P](from: ProcessId, to: ProcessId, payload: P): PLSend[P] = new PLSend(Packet(from,to,payload,Instance.ANY))
  }

  case class PLSend[P](packet: Packet[P])

  case class PLDeliver[P](packet: Packet[P])

  type PLModule[P] = ModS[StubLink[P]] {
    type Req = PLSend[P]
    type Ind = PLDeliver[P]
    type S = PLState[P]
  }

  case class PLState[P](delivered: Set[Packet[P]], module: Module[StubLink[P]]) extends StateWithModule[StubLink[P],PLState[P]] {
    def alreadyDelivered(d: Packet[P]): Boolean = delivered contains d

    def deliver(p: Packet[P]) = copy(delivered + p)

    override def updateModule(m: Module[StubLink[P]]): PLState[P] = copy(module = m)
  }


  def processLocal[P]: ProcessLocal[PLSend[P], PLState[P], PLDeliver[P], SLSend[P], SLDeliver[P], P] = {
    import oss.giussi.cappio.Messages._
    (msg, state) =>
      msg match {
        case PublicRequest(PLSend(p)) => LocalStep.withRequests(Set(LocalRequest(SLSend(p))), state)
        case LocalIndication(SLDeliver(p)) if !state.alreadyDelivered(p) => LocalStep.withIndications(Set(PLDeliver(p)), state.deliver(p))
        case _ => LocalStep.withState(state)
      }
  }
    def init[P] = apply[P] _

  def apply[P](timeout: Int): Module[PLModule[P]] = AbstractModule.mod[PLModule[P],PLModule[P]#Dep](PLState(Set.empty[Packet[P]],StubbornLink.init(timeout)),processLocal[P])

}
