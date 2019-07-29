package oss.giussi.cappio.impl.register

import oss.giussi.cappio.Messages.LocalStep
import oss.giussi.cappio._
import oss.giussi.cappio.impl.bcast.BestEffortBroadcast
import oss.giussi.cappio.impl.bcast.BestEffortBroadcast.{BebBcast, BebDeliver, BebMod}
import oss.giussi.cappio.impl.net.PerfectLink.{PLDeliver, PLSend}
import oss.giussi.cappio.impl.net.PerfectLinkBeta
import oss.giussi.cappio.impl.net.PerfectLinkBeta.PLModule
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


  case class ONRRState[V](state: ONRRStateI[V], module: Module[ONRRDep[V]]) extends StateWithModule[ONRRDep[V], ONRRState[V]] {
    override def updateModule(m: Module[ONRRDep[V]]): ONRRState[V] = copy(module = m)

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

    def init[V](self: ProcessId, all: Set[ProcessId], timeout: Int): ONRRState[V] = {
      val pl = PerfectLinkBeta.init[ONDirect[V]](timeout)
      val beb = BestEffortBroadcast.init[ONOp[V]](self, all, timeout)
      val cm = CombinedModule.paired(OneNRegularRegister.BEB, beb, OneNRegularRegister.PL, pl)
      val st = ONRRStateI(None, 0, 0, 0, 0, Map.empty[ProcessId, (Int, V)])
      ONRRState(st, cm)
    }
  }

  def init[V](self: ProcessId, N: Int, timeout: Int, all: Set[ProcessId]) = OneNRegularRegister[V](self, N, ONRRState.init(self, all, timeout))

  def processLocal[V](N: Int, self: ProcessId)(implicit inj1: Inject[ONRRDep[V]#Req,ONRRDep[V]#Dep1#Req], inj2: Inject[ONRRDep[V]#Req,ONRRDep[V]#Dep2#Req]) = new ProcessLocalHelper2[ONRRMod[V], ONRRDep[V]] {

    override def onPublicRequest(req: ONRRReq[V], state: State): Output = req match {
      case ONRRWrite(v) =>
        val ns = state.write()
        val beb = Set(req1(BebBcast(Payload(ONWRITE(ns.state.wts, v)), OneNRegularRegister.BEB)))
        LocalStep.withRequests(beb, ns)
      case ONRRRead =>
        val ns = state.read()
        val beb = Set(req1(BebBcast(Payload(ONREAD(ns.state.rid)), OneNRegularRegister.BEB)))
        LocalStep.withRequests(beb, ns)
    }

    override def onDependencyIndication1(ind: BebDeliver[ONOp[V]], state: State): Output = ind match {
        case BebDeliver(p, Payload(_, ONREAD(rid))) =>
          val send = Set(req2(PLSend(Packet(self, p, ONVALUE(rid, state.state.ts, state.state.value), OneNRegularRegister.PL)))) // lo tengo que meter en un Payload?
          LocalStep.withRequests(send, state)
        case BebDeliver(p, Payload(_, ONWRITE(ts, v))) =>
          val ns = state.deliver(ts, v)
          val ack = Set(req2(PLSend(Packet(self, p, ONACK(ts), OneNRegularRegister.PL)))) // lo tengo q meter en un payload?
          LocalStep.withRequests(ack, ns)
      }

    override def onDependencyIndication2(ind: PLDeliver[ONDirect[V]], state: State): Output = ind match {
      case PLDeliver(Packet(_, ONACK(ts), _, _, _)) =>
        if (state.state.wts == ts) { // TODO such that condition
          val (ns, res) = state.acked(N)
          val ind: Set[ONRRInd[V]] = if (res) Set(ONRRWriteReturn) else Set.empty
          LocalStep.withIndications(ind, ns)
        }
        else LocalStep.withState(state)
      case PLDeliver(Packet(_, ONVALUE(r, ts, v: Option[V]), q, _, _)) =>
        if (r == state.state.rid && v.isDefined) { // TODO v.isDefined?
          val (maybeValue, ns) = state.value(N, ts, v.get, q)
          val rr: Set[ONRRInd[V]] = maybeValue.map(ONRRReadReturn(_)).toSet
          LocalStep.withIndications(rr, ns)
        }
        else LocalStep.withState(state)
    }
  }
  /*
    def processLocal2[V](N: Int, self: ProcessId): ProcessLocal[ONRRReq[V], ONRRState[V], ONRRInd[V], ONRRDep#Req, ONRRDep#Ind] = (msg, state) => msg match {

    case PublicRequest(ONRRWrite(v)) =>
      val ns = state.write()
      val beb: Set[LocalRequest[ModuleReq]] = Set(LocalRequest(Left(BebBcast(Payload(ONWRITE(ns.state.wts, v)), OneNRegularRegister.BEB))))
      LocalStep.withRequests(beb, ns)
    case LocalIndication(Left(BebDeliver(p, Payload(_, ONWRITE(ts, v: V))))) => // FIXME type T is erased. Also Payload can be Anything!
      val ns = state.deliver(ts, v)
      val ack: Set[LocalRequest[ModuleReq]] = Set(LocalRequest(Right(PLSend(Packet(self, p, Payload(ONACK(ts)), OneNRegularRegister.PL)))))
      LocalStep.withRequests(ack, ns)
    case LocalIndication(Right(PLDeliver(Packet(_, ONACK(ts), _, _, _)))) =>
      if (state.state.wts == ts) { // TODO such that condition
        val (ns, res) = state.acked(N)
        val ind: Set[ONRRInd[V]] = if (res) Set(ONRRWriteReturn) else Set.empty
        LocalStep.withIndications(ind, ns)
      }
      else LocalStep.withState(state)
    case PublicRequest(ONRRRead) =>
      val ns = state.read()
      val beb: Set[LocalRequest[ModuleReq]] = Set(LocalRequest(Left(BebBcast(Payload(ONREAD(ns.state.rid)), OneNRegularRegister.BEB))))
      LocalStep.withRequests(beb, ns)
    case LocalIndication(Left(BebDeliver(p, Payload(_, ONREAD(rid))))) =>
      val send: Set[LocalRequest[ModuleReq]] = Set(LocalRequest(Right(PLSend(Packet(self, p, Payload(ONVALUE(rid, state.state.ts, state.state.value)), OneNRegularRegister.PL)))))
      LocalStep.withRequests(send, state)
    case LocalIndication(Right(PLDeliver(Packet(_, ONVALUE(r, ts, v: Option[V]), q, _, _)))) =>
      if (r == state.state.rid && v.isDefined) { // TODO v.isDefined?
        val (maybeValue, ns) = state.value(N, ts, v.get, q)
        val rr: Set[ONRRInd[V]] = maybeValue.map(ONRRReadReturn(_)).toSet
        LocalStep.withIndications(rr, ns)
      }
      else LocalStep.withState(state)
    case _ => LocalStep.withState(state) // TODO shouldn't happen.


    case _ => ???
  }

   */
}

// Majority voting regular register pag 147
case class OneNRegularRegister[V](self: ProcessId, N: Int, state: ONRRState[V]) extends AbstractModule[ONRRMod[V], ONRRDep[V]] {

  override def copyModule(s: ONRRState[V]) = copy(state = s)

  override val processLocal = OneNRegularRegister.processLocal[V](N, self)

}