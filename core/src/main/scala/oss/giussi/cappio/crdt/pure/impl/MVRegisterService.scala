package oss.giussi.cappio.crdt.pure.impl

import oss.giussi.cappio.crdt.Versioned
import oss.giussi.cappio.crdt.pure.CRDTTypes._
import oss.giussi.cappio.crdt.pure.CvRDTPureOpSimple

object MVRegisterService {

  def zero(): SimpleCRDT = MVRegisterServiceOps.zero

  implicit def MVRegisterServiceOps[A] = new CvRDTPureOpSimple[Set[A]] {

    override protected def customEval(ops: Seq[Versioned[Operation]]): Set[A] = ops.map(_.value.asInstanceOf[AssignOp].value.asInstanceOf[A]).toSet

    val r: Redundancy[Operation] = (op, _) => op.value.isInstanceOf[ClearOp.type]

    val r0: Redundancy_[Operation] = op1 => op2 => op2.vectorTimestamp < op1.vectorTimestamp

    override implicit val causalRedundancy: CausalRedundancy[Operation] = new CausalRedundancy[Operation](r, r0)

    override val optimizedUpdateState: PartialFunction[(Operation, Seq[Operation]), Seq[Operation]] = {
      case (ClearOp, _) => Seq.empty
      case (_, state)   => state
    }

  }

}

/**
 * Persistent assign operation used for MVRegister and LWWRegister.
 */
case class AssignOp(value: Any) // TODO type payload