package oss.giussi.cappio.crdt.pure

import org.scalatest.Matchers
import org.scalatest.WordSpec
import oss.giussi.cappio.crdt.pure.CRDTTestDSL.{AWCartCRDT, LWWRegisterCRDT, MVRegisterCRDT, VectorTimeControl}
import oss.giussi.cappio.crdt.pure.impl.AWSetService

class CRDTStableSpec extends WordSpec with Matchers {

  val mvReg = MVRegisterCRDT.ops.zero
  val lwwReg = LWWRegisterCRDT.ops.zero
  val awCart = AWCartCRDT.ops.zero
  val awSet = AWSetService.zero[Int]

  "An AWSet" should {
    import CRDTTestDSL.AWSetCRDT._
    "discard stable operations" in new VectorTimeControl {
      val updated = awSet
        .add(1, vt(1, 0))
        .add(2, vt(2, 0))
        .add(3, vt(2, 1))
        .add(4, vt(3, 1))
        .add(5, vt(3, 2))
        .stable(stableVT(2, 1))
      updated.value shouldBe Set(1, 2, 3, 4, 5)
      updated.polog.log.size shouldBe 2
      updated.state.size shouldBe 3
    }
    "remove stable values" in new VectorTimeControl {
      val updated = awSet
        .add(1, vt(1, 0))
        .stable(stableVT(1, 0))
        .remove(1, vt(2, 0))
      updated.value shouldBe Set()
    }
    "remove only stable values" in new VectorTimeControl {
      val updated = awSet
        .add(1, vt(1, 0))
        .add(2, vt(2, 0))
        .remove(1, vt(3, 0))
        .stable(stableVT(1, 0))
      updated.value shouldBe Set(2)
    }
    "clear stable values" in new VectorTimeControl {
      val updated = awSet
        .add(1, vt(1, 0))
        .add(2, vt(0, 1))
        .stable(stableVT(1, 0))
        .clear(vt(2, 0))
      updated.value shouldBe Set(2)
    }
  }

  "A MVRegister" should {
    import CRDTTestDSL.MVRegisterCRDT._
    "discard stable operations" in new VectorTimeControl {
      val updated = mvReg
        .assign(1, vt(1, 0))
        .assign(2, vt(0, 1))
        .stable(stableVT(1, 1))
      updated.value should be(Set(1, 2))
      updated.polog.log.size shouldBe 0
      updated.state.size shouldBe 2
    }
    "clear stable operations" in new VectorTimeControl {
      mvReg
        .assign(1, vt(1, 0))
        .assign(2, vt(0, 1))
        .stable(stableVT(1, 1))
        .clear(vt(2, 1))
        .value shouldBe Set()

    }
  }

  "A LWWRegister" should {
    import CRDTTestDSL.LWWRegisterCRDT._
    "discard stable operations" in new VectorTimeControl {
      val updated = lwwReg
        .assign(1, vt(1, 0), 0, "emitter1")
        .assign(2, vt(0, 1), 1, "emitter2")
        .assign(3, vt(2, 0), 2, "emitter2")
        .stable(stableVT(1, 1))
      updated.value should be(Some(3))
      updated.polog.log.size shouldBe 1
      updated.state.size shouldBe 1
    }
    "clear stable operations" in new VectorTimeControl {
      val updated = lwwReg
        .assign(1, vt(1, 0), 0, "emitter1")
        .assign(2, vt(0, 1), 1, "emitter2")
        .stable(stableVT(0, 1))
        .clear(vt(0, 2))
      updated.value shouldBe Some(1)
      updated.polog.log.size shouldBe 1
      updated.state.size shouldBe 0
    }
  }

  "An AWCart" should {
    import CRDTTestDSL.AWCartCRDT._
    "discard stable operations" in new VectorTimeControl {
      val updated = awCart
        .add("a", 1, vt(1, 0))
        .add("b", 2, vt(2, 0))
        .add("a", 5, vt(0, 1))
        .stable(stableVT(1, 1))
      updated.value should be(Map("a" -> 6, "b" -> 2))
      updated.polog.log.size shouldBe 1
      updated.state.size shouldBe 2
    }
    "clear stable operations" in new VectorTimeControl {
      val updated = awCart
        .add("a", 1, vt(1, 0))
        .add("b", 2, vt(2, 0))
        .stable(stableVT(2, 0))
        .clear(vt(3, 0))
      updated.value should be(Map())
      updated.polog.log.size shouldBe 0
      updated.state.size shouldBe 0
    }
  }

}

