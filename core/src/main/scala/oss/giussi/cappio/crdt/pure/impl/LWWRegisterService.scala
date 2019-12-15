package oss.giussi.cappio.crdt.pure.impl

import oss.giussi.cappio.crdt.Versioned
import oss.giussi.cappio.crdt.pure.CRDTTypes._
import oss.giussi.cappio.crdt.pure.CvRDTPureOpSimple

object LWWRegisterService {

  def zero(): SimpleCRDT = LWWRegisterServiceOps.zero

  implicit def LWWOrdering[A] = new Ordering[Versioned[_]] {
    override def compare(x: Versioned[_], y: Versioned[_]): Int =
      if (x.systemTimestamp == y.systemTimestamp)
        x.creator.compareTo(y.creator)
      else
        x.systemTimestamp.compareTo(y.systemTimestamp)
  }

  implicit def LWWRegisterServiceOps[A] = new CvRDTPureOpSimple[Option[A]] {

    override def customEval(ops: Seq[Versioned[Operation]]): Option[A] =
      ops.sorted(LWWRegisterService.LWWOrdering[A]).lastOption.map(_.value.asInstanceOf[AssignOp].value.asInstanceOf[A])

    val r: Redundancy[Operation] = (op, _) => op.value equals ClearOp

    val r0: Redundancy_[Operation] = newOp => op => op.vectorTimestamp < newOp.vectorTimestamp

    override implicit val causalRedundancy: CausalRedundancy[Operation] = new CausalRedundancy(r, r0)

  }

}