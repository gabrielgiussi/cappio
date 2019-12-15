package oss.giussi.cappio.crdt.pure.impl

import oss.giussi.cappio.crdt.VectorTime
import oss.giussi.cappio.crdt.pure.CRDTServiceOps
import oss.giussi.cappio.crdt.pure.CRDTTypes._

import scala.collection.immutable.Set

object TPSetService {

  type TPSet[A] = (Set[A], Set[A])

  def zero[A]: TPSet[A] = (Set.empty[A], Set.empty[A])

  implicit def TPSetServiceOps[A] = new CRDTServiceOps[TPSet[A], Set[A],SetOp] {

    override def zero: TPSet[A] = TPSetService.zero[A]

    override def eval(crdt: TPSet[A]): Set[A] = crdt._1

    override def effect(crdt: TPSet[A], op: SetOp, vt: VectorTime, systemTimestamp: Long = 0L, creator: String = ""): (Set[A], Set[A]) =
      (op, crdt) match {
        case (AddOp(e: A @unchecked), (added, removed)) if !removed.contains(e) => (added + e, removed)
        case (RemoveOp(e: A @unchecked), (added, removed)) => (added - e, removed + e)
        case (_, c) => c
      }

    override def precondition: Boolean = false
  }

}