package oss.ggiussi.cappio.impl.arbiter

import oss.ggiussi.cappio.InstanceID
import oss.ggiussi.cappio.core.Steps.Steps
import oss.ggiussi.cappio.core._
import oss.ggiussi.cappio.core.Transition.Transition


object Arbiter {

  implicit class ArbiterProtocol(id: Int) {
    def receivesReqFrom(from: Int) = ArbiterAction(ReceiveRequest(from, id))

    def sendsReqTo(to: Int) = ArbiterAction(SendRequest(id, to))

    def receivesGrantFrom(from: Int) = ArbiterAction(ReceiveGrant(from, id))

    def sendsGrantTo(to: Int) = ArbiterAction(SendGrant(id, to))
  }

}

sealed trait ArbiterActionHeader extends ActionHeader {
  override val instance = InstanceID("arbiter")
}

case class ArbiterAction(header: ArbiterActionHeader) extends Action {
  val payload: Option[Any] = None
}

case class ReceiveRequest(from: Int, id: Int) extends ArbiterActionHeader

case class ReceiveGrant(from: Int, id: Int) extends ArbiterActionHeader

case class SendRequest(id: Int, to: Int) extends ArbiterActionHeader

case class SendGrant(id: Int, to: Int) extends ArbiterActionHeader

object ArbiterState {
  // Hay una logica en la creacion del estado inicial (validar que el lastwordard apunte en la direccion del holder) TODO
  def initial(holding: Boolean, lastforward: Int) = ArbiterState(requesting = Set.empty, lastforward = lastforward, holding = holding, requested = false)
}

case class ArbiterState(requesting: Set[Int], lastforward: Int, holding: Boolean, requested: Boolean)

case class Arbiter(a: Int, neighbors: Set[Int]) extends Automaton[ArbiterState] {
  // action signature depends on the neighbors (the graph G)
  override val sig: ActionSignature = {
    val in: Set[ActionHeader] = neighbors.flatMap(v => Set(ReceiveRequest(v, a), ReceiveGrant(v, a)))
    val out: Set[ActionHeader] = neighbors.flatMap(v => Set(SendRequest(a, v), SendGrant(a, v)))
    ActionSignature(in, out, Set())
  }

  val steps: Steps[ArbiterState] = Steps.steps2(sig, action => action.header match {
    case ReceiveRequest(v, _) => Effect(state => state.copy(state.requesting + v))
    case ReceiveGrant(v, _) => Effect({ case state@ArbiterState(_, lastforward, holding, _) => if (!holding && lastforward == v) state.copy(holding = true, requested = false) else state })
    case SendRequest(_, v) => Effect({ case ArbiterState(requesting, lastforward, holding, requested) => !requesting.isEmpty && !requested && !holding && lastforward == v }, _.copy(requested = true))
    // TODO lastforward = w, y ∉ requesting for all y ∈ (w v) pag 64
    case SendGrant(_, v) => Effect({ case ArbiterState(requesting, lastforward, holding, _) => requesting.contains(v) && holding }, state => state.copy(state.requesting - v, lastforward = v, holding = false))
  })

}


case class ArbiterMessage(from: Int, to: Int, t: String)

class MessageSystem(adjacencies: Set[(Int, Int)]) extends Automaton[Set[ArbiterMessage]] {
  override val sig: ActionSignature = {
    val in: Set[ActionHeader] = adjacencies.flatMap { case (a1, a2) => Set(SendRequest(a1, a2), SendRequest(a2, a1), SendGrant(a1, a2), SendGrant(a2, a1)) }
    val out: Set[ActionHeader] = adjacencies.flatMap { case (a1, a2) => Set(ReceiveRequest(a1, a2), ReceiveRequest(a2, a1), ReceiveGrant(a1, a2), ReceiveGrant(a2, a1)) }
    ActionSignature(in, out, Set())
  }
  override val steps: Steps[Set[ArbiterMessage]] = Steps.steps2(sig, action => action.header match {
    case SendRequest(a, _a) if adjacencies contains(a, _a) => Effect(_ + ArbiterMessage(a, _a, "request"))
    case SendGrant(a, _a) if adjacencies contains(a, _a) => Effect(_ + ArbiterMessage(a, _a, "grant"))
    case ReceiveRequest(a, _a) if adjacencies contains(a, _a) => Effect(_ contains (ArbiterMessage(a, _a, "request")), _ - ArbiterMessage(a, _a, "request"))
    case ReceiveGrant(a, _a) if adjacencies contains(a, _a) => Effect(_ contains (ArbiterMessage(a, _a, "grant")), _ - ArbiterMessage(a, _a, "grant"))
  })
}

object Prueba extends App {

  /*
             a0 -- a1  -- a2
            /              \
          u3                u4
   */

  val arbiter = new Arbiter(0, Set(1, 3))
  val initialState0 = ArbiterState.initial(true, 1)

  val arbiter1 = new Arbiter(1, Set(0, 2))
  val initialState1 = ArbiterState.initial(holding = false, lastforward = 0)

  val arbiter2 = new Arbiter(2, Set(1, 4))
  val initialState2 = ArbiterState.initial(false, 1)

  implicit def f[S1, S2](s1: S1, s2: S2): (S1, S2) = (s1, s2)

  val m = new MessageSystem(Set(
    (0, 1), (1, 0),
    (1, 2), (2, 1)
  ))

  import Arbiter._

  val t = {
    val t0 = arbiter.compose(arbiter1, (s1: ArbiterState, s2: ArbiterState) => (s1, s2))(_._1, _._2).get
    t0.composeTuple(arbiter2).get
  }
  val automaton: Automaton[(((ArbiterState, ArbiterState), ArbiterState), Set[ArbiterMessage])] = t.composeTuple(m).get.hide(t.sig.out -- Set(SendGrant(0, 3), SendGrant(2, 4)))
  val initialState = (((initialState0, initialState1), initialState2), Set.empty[ArbiterMessage])

  println(Execution(automaton, initialState).next(2.receivesReqFrom(4)).isEnabled(1.receivesReqFrom(2)))

  val e = Execution(automaton, initialState).next(2.receivesReqFrom(4))
    .next(2.sendsReqTo(1))
    .next(1.receivesReqFrom(2))
    .next(1.sendsReqTo(0))
    .next(0.receivesReqFrom(1))
    .next(0.sendsGrantTo(1))
    .next(0.receivesReqFrom(3))
    .next(1.receivesGrantFrom(0))
    .next(1.sendsGrantTo(2))
    .next(2.receivesGrantFrom(1))
    .next(2.sendsGrantTo(4))

  e.sched().zipWithIndex.foreach(println)

}