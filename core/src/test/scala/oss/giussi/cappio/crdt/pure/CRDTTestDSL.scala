package oss.giussi.cappio.crdt.pure

import oss.giussi.cappio.crdt.VectorTime
import oss.giussi.cappio.crdt.pure.CRDTTypes.SimpleCRDT
import oss.giussi.cappio.crdt.pure.StabilityProtocol.TCStable
import oss.giussi.cappio.crdt.pure.impl.AWSetService.AWSet
import oss.giussi.cappio.crdt.pure.impl.TPSetService.TPSet
import oss.giussi.cappio.crdt.pure.impl.{AWCartEntry, AWCartService, AWSetService, AddOp, AssignOp, ClearOp, CounterService, LWWRegisterService, MVRegisterService, RemoveOp, TPSetService, UpdateOp}

import scala.collection.immutable.Set

object CRDTTestDSL {

  trait EnhancedCRDT[C] {
    def crdt: C

    def eval[B](implicit ops: CRDTServiceOps[C, B]): B = ops.eval(crdt)

    def value[B](implicit ops: CRDTServiceOps[C, B]): B = eval(ops)
  }

  trait Stable[C] {
    def crdt: C

    def stable(stable: TCStable)(implicit ops: CRDTServiceOps[C, _]) = ops.stable(crdt, stable)
  }

  trait Clear[C] {
    def crdt: C
    def clear(t: VectorTime)(implicit ops: CRDTServiceOps[C, _]) = ops.effect(crdt, ClearOp, t)
  }

  class SetCRDT[C](crdt: C) {

    def add[A](value: A, vectorTime: VectorTime)(implicit ops: CRDTServiceOps[C, _]) = ops.effect(crdt, AddOp(value), vectorTime)
    def remove[A](value: A, vectorTime: VectorTime)(implicit ops: CRDTServiceOps[C, _]) = ops.effect(crdt, RemoveOp(value), vectorTime)
  }

  class RegisterCRDT[C](crdt: C) {
    def assign[A](value: A, vectorTime: VectorTime, timestamp: Long = 0L, creator: String = "")(implicit ops: CRDTServiceOps[C, _]) = ops.effect(crdt, AssignOp(value), vectorTime, timestamp, creator)
    def clear(t: VectorTime)(implicit ops: CRDTServiceOps[C, _]) = ops.effect(crdt, ClearOp, t)
  }

  trait VectorTimeControl {
    var emitted = Set.empty[VectorTime]
    var stable = Set.empty[VectorTime]

    private def _vt(t1: Long, t2: Long) = VectorTime("p1" -> t1, "p2" -> t2)

    def vt(t1: Long, t2: Long): VectorTime = {
      val newVT = _vt(t1, t2)
      if (emitted.contains(newVT)) throw new RuntimeException(s"you are trying to add $newVT twice")
      stable.find(_ <-> newVT).foreach(st => throw new RuntimeException(s"you are trying to add a $newVT but is concurrent to the stable $st"))
      emitted += newVT
      newVT
    }

    def stableVT(t1: Long, t2: Long): TCStable = {
      val newVT = _vt(t1, t2)
      stable += newVT
      TCStable(newVT)
    }

    def clearVTHistory() = {
      stable = Set.empty
      emitted = Set.empty
    }
  }

  object MVRegisterCRDT {
    implicit val ops = MVRegisterService.MVRegisterServiceOps
    implicit class MVRegisterCRDT(val crdt: SimpleCRDT) extends RegisterCRDT(crdt) with Stable[SimpleCRDT] with EnhancedCRDT[SimpleCRDT]
  }

  object LWWRegisterCRDT {
    implicit val ops = LWWRegisterService.LWWRegisterServiceOps
    implicit class LWWRegisterCRDT(val crdt: SimpleCRDT) extends RegisterCRDT(crdt) with Stable[SimpleCRDT] with EnhancedCRDT[SimpleCRDT]
  }

  object CounterCRDT {
    implicit def ops[A: Integral] = CounterService.CounterServiceOps[A]
    implicit class CounterCRDT[A: Integral](val crdt: A) extends EnhancedCRDT[A] {
      def update(delta: A, vt: VectorTime)(implicit ops: CRDTServiceOps[A, A]) = ops.effect(crdt, UpdateOp(delta), vt)
    }
  }

  object AWSetCRDT {
    implicit def ops[A] = AWSetService.AWSetServiceOps[A]
    implicit class AWSetCRDT[A](val crdt: AWSet[A]) extends SetCRDT[AWSet[A]](crdt) with EnhancedCRDT[AWSet[A]] with Clear[AWSet[A]] with Stable[AWSet[A]]
  }

  object TPSetCRDT {
    implicit def ops[A] = TPSetService.TPSetServiceOps[A]
    implicit class TPSetCRDT[A](val crdt: TPSet[A]) extends SetCRDT[TPSet[A]](crdt) with EnhancedCRDT[TPSet[A]]
  }

  object AWCartCRDT {
    implicit def ops[A] = AWCartService.AWCartServiceOps[A]
    implicit class AWCartCRDT[A](val crdt: SimpleCRDT) extends Clear[SimpleCRDT] with Stable[SimpleCRDT] with EnhancedCRDT[SimpleCRDT] {
      def add(key: A, quantity: Int, timestamp: VectorTime) = ops.effect(crdt, AddOp(AWCartEntry(key, quantity)), timestamp)
      def remove(key: A, t: VectorTime) = ops.effect(crdt, RemoveOp(key), t)
    }
  }

}