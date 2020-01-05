package oss.giussi.cappio.impl.register

import oss.giussi.cappio.Messages.LocalStep
import oss.giussi.cappio._
import oss.giussi.cappio.impl.bcast.BestEffortBroadcast
import oss.giussi.cappio.impl.bcast.BestEffortBroadcast.{BebBcast, BebDeliver, BebMod}
import oss.giussi.cappio.impl.net.PerfectLink
import oss.giussi.cappio.impl.net.PerfectLink.{PLDeliver, PLModule, PLSend}
import oss.giussi.cappio.impl.register.OneNRegularRegister._
import shapeless.ops.coproduct.Inject

object OneNRegularRegister {

  val BEB = Instance("beb")
  val PL = Instance("pl")

  sealed trait ONRRReq[+V]

  case object ONRRRead extends ONRRReq[Nothing]

  case class ONRRWrite[V](v: V) extends ONRRReq[V]

  sealed trait ONRRInd[+V]

  // TODO or option? que pasa si hace un read y todavia no hicieron write?
  case class ONRRReadReturn[V](v: V) extends ONRRInd[V]

  case object ONRRWriteReturn extends ONRRInd[Nothing]

  type ONRRDep[P] = Mod2 {
    type Dep1 = BebMod[ONOp[P]]
    type Dep2 = PLModule[ONDirect[P]]
    type State = (Dep1#State, Dep2#State) // (BebMod[ONOp[P]]#State,PLModule[ONDirect[P]]#State)
  }

  type ONRRMod[V] = ModS[ONRRDep[V]] {
    type Req = ONRRReq[V]
    type S = ONRRState[V]
    type Ind = ONRRInd[V]
  }

  type ReadList[V] = Map[ProcessId, (Int, V)]

  sealed trait ONOp[+V]

  case class ONREAD(rid: Int) extends ONOp[Nothing]

  case class ONWRITE[V](timestamp: Int, v: V) extends ONOp[V]

  sealed trait ONDirect[+V]

  case class ONACK(timestamp: Int) extends ONDirect[Nothing]

  case class ONVALUE[V](r: Int, ts: Int, v: Option[V]) extends ONDirect[V]

  case class ONRRStateI[V](value: Option[V], ts: Int, wts: Int, acks: Int, rid: Int, readlist: ReadList[V]) {

    def write() = copy(acks = 0, wts = wts + 1)

    def deliver(timestamp: Int, newValue: V) = copy(ts = timestamp, value = Some(newValue))

    // TODO move
    // // esta bien la division?
    def acked(n: Int) = if (acks + 1 > n / 2) (copy(acks = 0), true) else (copy(acks = acks + 1), false)

    def read() = copy(rid = rid + 1, readlist = Map.empty)

    def value(n: Int, ts: Int, v: V, q: ProcessId): (Option[V], ONRRStateI[V]) = {
      val rl = readlist + (q -> (ts, v))
      if (rl.size > n / 2) (Some(rl.maxBy(_._2._1)._2._2), copy(readlist = Map.empty))
      else (None, copy(readlist = rl))
    }
  }


  case class ONRRState[V](state: ONRRStateI[V]) {
    def write() = copy(state = state.write())

    def deliver(timestamp: Int, newValue: V) = if (timestamp > state.ts) copy(state = state.deliver(timestamp, newValue)) else this

    def acked(n: Int) = {
      val (ns, res) = state.acked(n)
      (copy(state = ns), res)
    }

    def read() = copy(state = state.read())

    def value(n: Int, ts: Int, v: V, q: ProcessId) = {
      val (maybeVal, ns) = state.value(n, ts, v, q)
      (maybeVal, copy(state = ns))
    }
  }

  object ONRRState {

    def init[V](self: ProcessId, all: Set[ProcessId], timeout: Int): StateWithModule[ONRRMod[V]#Dep,ONRRMod[V]#S] = {
      val pl = PerfectLink.init[ONDirect[V]](timeout)
      val beb = BestEffortBroadcast[ONOp[V]](all, timeout)(self)
      val cm = CombinedModule.paired(OneNRegularRegister.BEB, beb, OneNRegularRegister.PL, pl)
      val st = ONRRStateI(None, 0, 0, 0, 0, Map.empty[ProcessId, (Int, V)])
      StateWithModule(cm,ONRRState(st))
    }
  }

  def processLocal[V](N: Int, self: ProcessId)(implicit inj1: Inject[ONRRDep[V]#Req,ONRRDep[V]#Dep1#Req], inj2: Inject[ONRRDep[V]#Req,ONRRDep[V]#Dep2#Req]) = new ProcessLocalHelper2[ONRRMod[V], ONRRDep[V]] {

    override def onPublicRequest(req: ONRRReq[V], state: State): Output = req match {
      case ONRRWrite(v) =>
        val ns = state.updateState(_.write)
        val beb = Set(req1(BebBcast(Payload(ONWRITE(ns.state.state.wts, v)), OneNRegularRegister.BEB)))
        LocalStep.withRequests(beb, ns)
      case ONRRRead =>
        val ns = state.updateState(_.read)
        val beb = Set(req1(BebBcast(Payload(ONREAD(ns.state.state.rid)), OneNRegularRegister.BEB)))
        LocalStep.withRequests(beb, ns)
    }

    override def onDependencyIndication1(ind: BebDeliver[ONOp[V]], state: State): Output = ind match {
        case BebDeliver(p, Payload(_, ONREAD(rid))) =>
          val send = Set(req2(PLSend(Packet(self, p, ONVALUE(rid, state.state.state.ts, state.state.state.value), OneNRegularRegister.PL))))
          LocalStep.withRequests(send, state)
        case BebDeliver(p, Payload(_, ONWRITE(ts, v))) =>
          val ns = state.updateState(_.deliver(ts, v))
          val ack = Set(req2(PLSend(Packet(self, p, ONACK(ts), OneNRegularRegister.PL))))
          LocalStep.withRequests(ack, ns)
      }

    override def onDependencyIndication2(ind: PLDeliver[ONDirect[V]], state: State): Output = ind match {
      case PLDeliver(Packet(_, ONACK(ts), _, _, _)) =>
        if (state.state.state.wts == ts) { // TODO such that condition
          val (ns, res) = state.state.acked(N)
          val ind: Set[ONRRInd[V]] = if (res) Set(ONRRWriteReturn) else Set.empty
          LocalStep.withIndications(ind, state.updateState(ns))
        }
        else LocalStep.withState(state)
      case PLDeliver(Packet(_, ONVALUE(r, ts, v: Option[V]), q, _, _)) =>
        if (r == state.state.state.rid && v.isDefined) { // TODO v.isDefined?
          val (maybeValue, ns) = state.state.value(N, ts, v.get, q)
          val rr: Set[ONRRInd[V]] = maybeValue.map(ONRRReadReturn(_)).toSet
          LocalStep.withIndications(rr, state.updateState(ns))
        }
        else LocalStep.withState(state)
    }
  }

  // Majority voting regular register pag 147
  def apply[P](all: Set[ProcessId], timeout: Int)(self: ProcessId): Module[ONRRMod[P]] = AbstractModule.mod[ONRRMod[P],ONRRMod[P]#Dep](ONRRState.init(self,all,timeout),processLocal(all.size,self))

}