package oss.giussi.cappio.crdt.pure

import org.scalatest.Matchers
import org.scalatest.WordSpecLike
import oss.giussi.cappio.crdt.VectorTime
import oss.giussi.cappio.crdt.pure.StabilityProtocol.{RTM, StabilityConf, TCStable}

class StabilityProtocolSpec extends WordSpecLike with Matchers {

  val A = "A"
  val B = "B"
  val C = "C"

  def partitions: Set[String] = Set(A, B, C)

  def initialRTM = RTM(StabilityConf(A, partitions))

  def vt(a: Long, b: Long, c: Long) = VectorTime(A -> a, B -> b, C -> c)

  def tcstable(a: Long, b: Long, c: Long) = Some(TCStable(vt(a, b, c)))

  "Stability" should {
    "drop updates from local partition" in {
      initialRTM
        .update(A, vt(1, 1, 1))
        .update(B, vt(2, 2, 2))
        .update(C, vt(2, 2, 2))
        .stable shouldBe tcstable(2, 2, 2)
    }
    "not emit tcstable when B = (1,1,1), C = unknown " in {
      initialRTM
        .update(B, vt(1, 1, 1))
        .stable shouldBe None
    }
    "emit TCStable(0,1) when B = (0,1,1), C = (0,0,1) " in {
      initialRTM
        .update(B, vt(0, 1, 1))
        .update(C, vt(0, 0, 1))
        .stable shouldBe tcstable(0, 0, 1)
    }
    "emit TCStable(1,1) when A = (1,1,1), B = (1,1,1)" in {
      initialRTM
        .update(B, vt(1, 1, 1))
        .update(C, vt(1, 1, 1))
        .stable shouldBe tcstable(1, 1, 1)
    }
    "emit TCStable(1,1) when A = (2,1), B = (1,2)" in {
      initialRTM
        .update(B, vt(2, 2, 1))
        .update(C, vt(1, 1, 2))
        .stable shouldBe tcstable(1, 1, 1)
    }
  }

}
