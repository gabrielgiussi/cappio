package oss.giussi.cappio.crdt.pure.impl

import oss.giussi.cappio.crdt.Versioned
import oss.giussi.cappio.crdt.pure.CRDTTypes._
import oss.giussi.cappio.crdt.pure.CvRDTPureOpSimple

object MVRegisterService {

  type MVRegister = SimpleCRDT[RegisterOp]

  def zero(): MVRegister = MVRegisterServiceOps.zero

  implicit def MVRegisterServiceOps[A] = new CvRDTPureOpSimple[Set[A],RegisterOp] {

    override protected def customEval(ops: Seq[Versioned[RegisterOp]]): Set[A] = ops.map(_.value).collect {
      case AssignOp(v) => v.asInstanceOf[A]
    }.toSet

    val r: Redundancy[RegisterOp] = (op, _) => op.value match {
      case ClearRegOp => true
      case _ => false
    }

    val r0: Redundancy_[RegisterOp] = op1 => op2 => op2.vectorTimestamp < op1.vectorTimestamp

    override implicit val causalRedundancy: CausalRedundancy[RegisterOp] = new CausalRedundancy(r, r0)

    override val optimizedUpdateState: PartialFunction[(RegisterOp, Seq[RegisterOp]), Seq[RegisterOp]] = {
      case (ClearRegOp, _) => Seq.empty
      case (_, state)   => state
    }

  }

}

sealed trait RegisterOp

/**
 * Persistent assign operation used for MVRegister and LWWRegister.
 */
case class AssignOp(value: Any) extends RegisterOp // TODO type payload

case object ClearRegOp extends RegisterOp