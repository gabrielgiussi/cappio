package oss.giussi.cappio.crdt.pure.impl

import oss.giussi.cappio.crdt.pure.{CRDT, CvRDTPureOp}
import oss.giussi.cappio.crdt.pure.CRDTTypes._
import oss.giussi.cappio.crdt.pure.impl.AWSetService.AWSet

import scala.collection.immutable.Set

object AWSetService {

  type AWSet[A] = CRDT[Set[A],SetOp]

  def zero[A]: AWSet[A] = CRDT(Set.empty)

  implicit def AWSetServiceOps[A] = new CvRDTPureOp[Set[A], Set[A], SetOp] {

    val r: Redundancy[SetOp] = (v, _) => v.value match {
      case _: RemoveOp => true
      case ClearOp     => true
      case _           => false
    }

    val r0: Redundancy_[SetOp] = newOp => op => {
      ((op.vectorTimestamp, op.value), (newOp.vectorTimestamp, newOp.value)) match {
        case ((t1, AddOp(v1)), (t2, AddOp(v2)))    => (t1 < t2) && (v1 equals v2)
        case ((t1, AddOp(v1)), (t2, RemoveOp(v2))) => (t1 < t2) && (v1 equals v2)
        case ((t1, AddOp(_)), (t2, ClearOp))       => t1 < t2
      }
    }

    override implicit val causalRedundancy: CausalRedundancy[SetOp] = new CausalRedundancy(r, r0)

    override def eval(crdt: AWSet[A]): Set[A] =
      crdt.polog.log.map(_.value.asInstanceOf[AddOp].entry.asInstanceOf[A]) ++ crdt.state

    override protected def stabilizeState(state: Set[A], stableOps: Seq[SetOp]): Set[A] =
      state ++ stableOps.map(_.asInstanceOf[AddOp].entry.asInstanceOf[A]).toSet

    override def zero: AWSet[A] = AWSetService.zero[A]

    override def updateState(op: SetOp, redundant: Boolean, state: Set[A]): Set[A] = op match {
      case RemoveOp(entry) => state - entry.asInstanceOf[A]
      case ClearOp         => Set.empty
      case _               => state
    }
  }
}

sealed trait SetOp

/**
 * Persistent add operation used for [[AWSet]] and AWCart.
 */
case class AddOp(entry: Any) extends SetOp

/**
 * Persistent remove operation used for [[AWSet]] and AWCart.
 */
case class RemoveOp(entry: Any) extends SetOp

case object ClearOp extends SetOp
