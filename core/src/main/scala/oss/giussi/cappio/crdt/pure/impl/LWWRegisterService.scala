package oss.giussi.cappio.crdt.pure.impl

import oss.giussi.cappio.crdt.Versioned
import oss.giussi.cappio.crdt.pure.CRDTTypes._
import oss.giussi.cappio.crdt.pure.CvRDTPureOpSimple

object LWWRegisterService {

  type LWWRegister = SimpleCRDT[RegisterOp]

  def zero(): LWWRegister = LWWRegisterServiceOps.zero

  implicit def LWWOrdering[A] = new Ordering[Versioned[_]] {
    override def compare(x: Versioned[_], y: Versioned[_]): Int =
      if (x.systemTimestamp == y.systemTimestamp)
        x.creator.compareTo(y.creator)
      else
        x.systemTimestamp.compareTo(y.systemTimestamp)
  }

  implicit def LWWRegisterServiceOps[A] = new CvRDTPureOpSimple[Option[A], RegisterOp] {

    override def customEval(ops: Seq[Versioned[RegisterOp]]): Option[A] =
      ops.sorted(LWWRegisterService.LWWOrdering[A]).lastOption.map(_.value.asInstanceOf[AssignOp].value.asInstanceOf[A]) // TODO remove asInstanceOf

    val r: Redundancy[RegisterOp] = (op, _) => op.value equals ClearRegOp

    val r0: Redundancy_[RegisterOp] = newOp => op => op.vectorTimestamp < newOp.vectorTimestamp

    override implicit val causalRedundancy: CausalRedundancy[RegisterOp] = new CausalRedundancy(r, r0)

  }

}