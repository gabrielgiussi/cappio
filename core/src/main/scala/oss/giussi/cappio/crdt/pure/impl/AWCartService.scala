package oss.giussi.cappio.crdt.pure.impl

import oss.giussi.cappio.crdt.Versioned
import oss.giussi.cappio.crdt.pure.CRDTTypes._
import oss.giussi.cappio.crdt.pure.{CRDTService, CvRDTPureOpSimple}

/**
 * AWCart entry.
 *
 * @param key      Entry key. Used to identify a product in the shopping cart.
 * @param quantity Entry quantity.
 * @tparam A Key type.
 */
case class AWCartEntry[A](key: A, quantity: Int)

object AWCartService {

  def zero(): SimpleCRDT = AWCartServiceOps.zero

  implicit def AWCartServiceOps[A] = new CvRDTPureOpSimple[Map[A, Int]] {

    override def customEval(ops: Seq[Versioned[Operation]]): Map[A, Int] = ops.foldLeft(Map.empty[A, Int]) {
      case (acc, Versioned(AddOp(AWCartEntry(key: A @unchecked, quantity)), _, _, _)) => acc.get(key) match {
        case Some(c) => acc + (key -> (c + quantity))
        case None    => acc + (key -> quantity)
      }
      case (acc, Versioned(RemoveOp(_), _, _, _)) => acc
    }

    val r: Redundancy = (v, _) => v.value match {
      case _: RemoveOp => true
      case ClearOp     => true
      case _           => false
    }

    val r0: Redundancy_ = newOp => op => {
      ((op.vectorTimestamp, op.value), (newOp.vectorTimestamp, newOp.value)) match {
        case ((t1, AddOp(AWCartEntry(k1, _))), (t2, RemoveOp(k2))) => (t1 < t2) && (k1 equals k2)
        case ((t1, AddOp(_)), (t2, ClearOp)) => t1 < t2
        case _ => false
      }
    }

    override implicit val causalRedundancy: CausalRedundancy = new CausalRedundancy(r, r0)

    override val optimizedUpdateState: PartialFunction[(Operation, Seq[Operation]), Seq[Operation]] = {
      case (RemoveOp(key), state) => state.filterNot(_.asInstanceOf[AWCartEntry[_]].key equals key)
      case (ClearOp, _)           => Seq.empty
      case (_, state)             => state
    }

  }
}

/**
 * Replicated AWCart CRDT service.
 *
 *  - For adding a new `key` of given `quantity` a client should call `add`.
 *  - For incrementing the `quantity` of an existing `key` a client should call `add`.
 *  - For decrementing the `quantity` of an existing `key` a client should call `remove`, followed by `add`
 * (after `remove` successfully completed).
 *  - For removing a `key` a client should call `remove`.
 *
 * @param serviceId Unique id of this service.
 * @tparam A AWCart key type.
 */
class AWCartService[A](val serviceId: String)
  extends CRDTService[SimpleCRDT, Map[A, Int]] {

  val ops = AWCartService.AWCartServiceOps[A]

  /**
   * Adds the given `quantity` of `key` to the OR-Cart identified by `id` and returns the updated OR-Cart content.
   */
  def add(id: String, key: A, quantity: Int): Map[A, Int] = ???

  /**
   * Removes the given `key` from the OR-Cart identified by `id` and returns the updated OR-Cart content.
   */
  def remove(id: String, key: A): Map[A, Int] = op(id, RemoveOp(key))

  def clear(id: String): Map[A, Int] = ???

  /**
   * Returns the current value of the CRDT identified by `id`.
   */
  override def value(id: String): Map[A, Int] = ???

  override protected def op(id: String, operation: Operation): Map[A, Int] = ???
}
