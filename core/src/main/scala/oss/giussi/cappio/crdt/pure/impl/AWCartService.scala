package oss.giussi.cappio.crdt.pure.impl

import oss.giussi.cappio.crdt.Versioned
import oss.giussi.cappio.crdt.pure.CRDTTypes._
import oss.giussi.cappio.crdt.pure.CvRDTPureOpSimple

/**
 * AWCart entry.
 *
 * @param key      Entry key. Used to identify a product in the shopping cart.
 * @param quantity Entry quantity.
 * @tparam A Key type.
 */
case class AWCartEntry[A](key: A, quantity: Int)

object AWCartService {

  type AWCart = SimpleCRDT[SetOp]

  def zero(): AWCart = AWCartServiceOps.zero

  implicit def AWCartServiceOps[A] = new CvRDTPureOpSimple[Map[A, Int], SetOp] {

    override def customEval(ops: Seq[Versioned[SetOp]]): Map[A, Int] = ops.foldLeft(Map.empty[A, Int]) {
      case (acc, Versioned(AddOp(AWCartEntry(key: A @unchecked, quantity)), _, _, _)) => acc.get(key) match {
        case Some(c) => acc + (key -> (c + quantity))
        case None    => acc + (key -> quantity)
      }
      case (acc, Versioned(RemoveOp(_), _, _, _)) => acc
    }

    val r: Redundancy[SetOp] = (v, _) => v.value match {
      case _: RemoveOp => true
      case ClearOp     => true
      case _           => false
    }

    val r0: Redundancy_[SetOp] = newOp => op => {
      ((op.vectorTimestamp, op.value), (newOp.vectorTimestamp, newOp.value)) match {
        case ((t1, AddOp(AWCartEntry(k1, _))), (t2, RemoveOp(k2))) => (t1 < t2) && (k1 equals k2)
        case ((t1, AddOp(_)), (t2, ClearOp)) => t1 < t2
        case _ => false
      }
    }

    override implicit val causalRedundancy: CausalRedundancy[SetOp] = new CausalRedundancy(r, r0)

    override val optimizedUpdateState: PartialFunction[(SetOp, Seq[SetOp]), Seq[SetOp]] = {
      case (RemoveOp(key), state) => state.filterNot(_.asInstanceOf[AWCartEntry[_]].key equals key)
      case (ClearOp, _)           => Seq.empty
      case (_, state)             => state
    }

  }
}