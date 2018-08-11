package oss.ggiussi.cappio.impl.arbiter

import oss.ggiussi.cappio.Steps.Steps
import oss.ggiussi.cappio._
import oss.ggiussi.cappio.Transition.Transition


case class ReceiveRequest(v: Int, a: Int) extends Action

case class ReceiveGrant(v: Int, a: Int) extends Action

case class SendRequest(a: Int, v: Int) extends Action

case class SendGrant(a: Int, v: Int) extends Action

object ArbiterState {
  // Hay una logica en la creacion del estado inicial (validar que el lastwordard apunte en la direccion del holder) TODO
  def initial(holding: Boolean, lastforward: Int) = ArbiterState(requesting = Set.empty,lastforward = lastforward, holding = holding, requested = false)
}

case class ArbiterState(requesting: Set[Int], lastforward: Int, holding: Boolean, requested: Boolean)



class Arbiter(a: Int, neighbors: Set[Int]) extends Automaton[ArbiterState] {

  val steps: Steps[ArbiterState] = {
    val transitions: Set[Transition[ArbiterState]] = Set(
      Transition.inputTransition[ArbiterState](
        {
          case ReceiveRequest(_, `a`) => true
          case _ => false
        }, (s: ArbiterState, a: Action) => s.copy(s.requesting + a.asInstanceOf[ReceiveRequest].v)
      )
    )
    Steps.steps(transitions)
  }

  // action signature depends on the neighbors (the graph G)
  override val sig: ActionSignature = {
    val in: Set[Action] = neighbors.flatMap(v => Set(ReceiveRequest(v, a), ReceiveGrant(v, a)))
    val out: Set[Action] = neighbors.flatMap(v => Set(SendRequest(a, v), SendGrant(a, v)))
    ActionSignature(in, out, Set())
  }

}

object Prueba extends App {

  val arbiter = new Arbiter(0,Set(1))
  val initialState0 = ArbiterState.initial(true,1)

  val arbiter1 = new Arbiter(1,Set(0))
  val initialState1 = ArbiterState.initial(false,0)

  val c = arbiter.compose(arbiter1,(s1: ArbiterState, s2: ArbiterState) => (s1,s2))(s => s._1, s => s._2).get
  val e = Execution(c, (initialState0,initialState1))

  println(e.state)
  println(e.next(ReceiveRequest(1,0)).next(ReceiveRequest(1,1)).state)


}
