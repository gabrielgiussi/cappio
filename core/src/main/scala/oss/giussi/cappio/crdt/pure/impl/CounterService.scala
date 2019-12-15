package oss.giussi.cappio.crdt.pure.impl

import oss.giussi.cappio.crdt.VectorTime
import oss.giussi.cappio.crdt.pure.CRDTServiceOps

object CounterService {

  def zero[A: Integral]: A = implicitly[Integral[A]].zero

  implicit def CounterServiceOps[A: Integral] = new CRDTServiceOps[A, A, UpdateOp] {

    override def zero: A = CounterService.zero[A]

    override def precondition: Boolean = false

    override def eval(crdt: A): A = crdt

    override def effect(crdt: A, op: UpdateOp, vt: VectorTime, systemTimestamp: Long, creator: String): A = implicitly[Integral[A]].plus(crdt, op.delta.asInstanceOf[A])

  }
}

/**
 * Persistent update operation used for Counter.
 */
case class UpdateOp(delta: Any) // TODO type
