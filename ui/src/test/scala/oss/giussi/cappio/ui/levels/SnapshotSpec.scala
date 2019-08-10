package oss.giussi.cappio.ui.levels

import org.scalatest.{Matchers, WordSpec}
import oss.giussi.cappio.impl.bcast.BestEffortBroadcast
import oss.giussi.cappio.impl.bcast.BestEffortBroadcast.{BebBcast, BebMod}
import oss.giussi.cappio.ui.core.Index
import oss.giussi.cappio._

class SnapshotSpec extends WordSpec with Matchers {

  val p0 = ProcessId(0)
  val p1 = ProcessId(1)
  val p3 = ProcessId(3)
  val p4 = ProcessId(4)
  val all = (0 to 4).map(ProcessId).toSet

  "Snapshot" must {
    "" ignore {
      val step = WaitingRequest(Scheduler.init(all,BestEffortBroadcast.init[String](all,3)))
      val next = Snapshot.next[BebMod[String]](_.toString,_.toString) _
      val req = RequestBatch[BebBcast[String]](Map.empty)
        .add(p0,BebBcast("a"))
        .add(p4,BebBcast("a"))
      val index0 = Index(0)
      val s1 = next(Snapshot(index0,List.empty,step, None),NextReq[BebMod[String]](req))
      val wd = s1.step.asInstanceOf[WaitingDeliver[BebMod[String]]]
      val delivers: List[Either[FLLDeliver[String],Drop[String]]] = wd.scheduler.network.inTransit.collect {
        case p if (p.packet.from == p0 && (p.packet.to == p3 || p.packet.to == p1)) => Left(p.deliver)
      }.toList
      val s2 = next(s1,NextDeliver[BebMod[String]](DeliverBatch[String](delivers : _*)))
      val s3 = next(s2,NextReq[BebMod[String]](RequestBatch[BebBcast[String]](Map.empty)))
      val wd2 = s3.step.asInstanceOf[WaitingDeliver[BebMod[String]]]
      val delivers2 = wd2.scheduler.network.inTransit.collect {
        case p if (p.packet.from == p4 && p.packet.to == p0) => Left(p.deliver)
        case p if (p.packet.from == p4 && p.packet.to == p4) => Right(p.drop)
      }
      val s4 = next(s3,NextDeliver[BebMod[String]](DeliverBatch[String](delivers : _*)))
      println(s4.actions.groupBy(_.id).values.map(_.size))

      s3.actions.foreach(println)
      println("######################")
      s4.actions.foreach(println)


    }
  }

}
