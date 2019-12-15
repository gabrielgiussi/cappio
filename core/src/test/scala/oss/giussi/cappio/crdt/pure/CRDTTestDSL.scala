package oss.giussi.cappio.crdt.pure

import oss.giussi.cappio.crdt.VectorTime
import oss.giussi.cappio.crdt.pure.StabilityProtocol.TCStable
import oss.giussi.cappio.crdt.pure.impl.AWCartService.AWCart
import oss.giussi.cappio.crdt.pure.impl.AWSetService.AWSet
import oss.giussi.cappio.crdt.pure.impl.LWWRegisterService.LWWRegister
import oss.giussi.cappio.crdt.pure.impl.MVRegisterService.MVRegister
import oss.giussi.cappio.crdt.pure.impl.TPSetService.TPSet
import oss.giussi.cappio.crdt.pure.impl.{AWCartEntry, AWCartService, AWSetService, AddOp, AssignOp, ClearOp, ClearRegOp, CounterService, LWWRegisterService, MVRegisterService, RegisterOp, RemoveOp, SetOp, TPSetService, UpdateOp}

import scala.collection.immutable.Set

object CRDTTestDSL {

  trait EnhancedCRDT[C] {
    def crdt: C

    def eval[B](implicit ops: CRDTServiceOps[C, B, _]): B = ops.eval(crdt)

    def value[B](implicit ops: CRDTServiceOps[C, B, _]): B = eval(ops)
  }

  trait Stable[C] {
    def crdt: C

    def stable(stable: TCStable)(implicit ops: CRDTServiceOps[C, _, _]) = ops.stable(crdt, stable)
  }

  trait Clear[C] {
    def crdt: C
    def clear(t: VectorTime)(implicit ops: CRDTServiceOps[C, _, SetOp]) = ops.effect(crdt, ClearOp, t)
  }

  class SetCRDT[C](crdt: C) {

    def add[A](value: A, vectorTime: VectorTime)(implicit ops: CRDTServiceOps[C, _, SetOp]) = ops.effect(crdt, AddOp(value), vectorTime)
    def remove[A](value: A, vectorTime: VectorTime)(implicit ops: CRDTServiceOps[C, _, SetOp]) = ops.effect(crdt, RemoveOp(value), vectorTime)
  }

  class RegisterCRDT[C](crdt: C) {
    def assign[A](value: A, vectorTime: VectorTime, timestamp: Long = 0L, creator: String = "")(implicit ops: CRDTServiceOps[C, _, RegisterOp]) = ops.effect(crdt, AssignOp(value), vectorTime, timestamp, creator)
    def clear(t: VectorTime)(implicit ops: CRDTServiceOps[C, _, RegisterOp]) = ops.effect(crdt, ClearRegOp, t)
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
    implicit class EnhancedMVRegisterCRDT(val crdt: MVRegister) extends RegisterCRDT(crdt) with Stable[MVRegister] with EnhancedCRDT[MVRegister]
  }

  object LWWRegisterCRDT {
    implicit val ops = LWWRegisterService.LWWRegisterServiceOps
    implicit class EnhancedLWWRegisterCRDT(val crdt: LWWRegister) extends RegisterCRDT(crdt) with Stable[LWWRegister] with EnhancedCRDT[LWWRegister]
  }

  object CounterCRDT {
    implicit def ops[A: Integral] = CounterService.CounterServiceOps[A]
    implicit class EnhancedCounterCRDT[A: Integral](val crdt: A) extends EnhancedCRDT[A] {
      def update(delta: A, vt: VectorTime)(implicit ops: CRDTServiceOps[A, A, UpdateOp]) = ops.effect(crdt, UpdateOp(delta), vt)
    }
  }

  object AWSetCRDT {
    implicit def ops[A] = AWSetService.AWSetServiceOps[A]
    implicit class EnhancedAWSetCRDT[A](val crdt: AWSet[A]) extends SetCRDT[AWSet[A]](crdt) with EnhancedCRDT[AWSet[A]] with Clear[AWSet[A]] with Stable[AWSet[A]]
  }

  object TPSetCRDT {
    implicit def ops[A] = TPSetService.TPSetServiceOps[A]
    implicit class EnhancedTPSetCRDT[A](val crdt: TPSet[A]) extends SetCRDT[TPSet[A]](crdt) with EnhancedCRDT[TPSet[A]]
  }

  object AWCartCRDT {
    implicit def ops[A] = AWCartService.AWCartServiceOps[A]
    implicit class EnhancedAWCartCRDT(val crdt: AWCart) extends Clear[AWCart] with Stable[AWCart] with EnhancedCRDT[AWCart] {
      def add[A](key: A, quantity: Int, timestamp: VectorTime) = ops.effect(crdt, AddOp(AWCartEntry(key, quantity)), timestamp)
      def remove[A](key: A, t: VectorTime) = ops.effect(crdt, RemoveOp(key), t)
    }
  }

}