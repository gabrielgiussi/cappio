package oss.ggiussi.cappio.core

import org.scalatest.{FlatSpec, Matchers}
import oss.ggiussi.cappio
import oss.ggiussi.cappio.InstanceID
import oss.ggiussi.cappio.impl.arbiter.Arbiter

class ArbiterSpec extends FlatSpec with Matchers {

  /*
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
    val automaton: Automaton[(((ArbiterState, ArbiterState), ArbiterState), Set[Message])] = t.composeTuple(m).get.hide(t.sig.out -- Set(SendGrant(0, 3), SendGrant(2, 4)))
    val initialState = (((initialState0, initialState1), initialState2), Set.empty[Message])

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
  */

  "Arbiter" should "compose with other arbiters" in {
    for {
      a1 <- Some(Arbiter(0, Set(1, 3)))
      a2 <- a1 composeTuple Arbiter(1, Set(0, 2))
      a3 <- a2 composeTuple Arbiter(2, Set(1, 4))
    } yield a3 should not be None

  }

}
