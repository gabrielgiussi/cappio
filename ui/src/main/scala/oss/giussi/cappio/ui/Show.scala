package oss.giussi.cappio.ui

import oss.giussi.cappio.crdt.pure.impl.{AddOp, ClearOp, RemoveOp, SetOp}
import oss.giussi.cappio.crdt.{VectorTime, Versioned}
import oss.giussi.cappio.impl.CRDTApp.{Add, CRDTMod, Remove, SetRequest}
import oss.giussi.cappio.impl.bcast.BestEffortBroadcast.BebMod
import oss.giussi.cappio.impl.bcast.CausalOrderReliableBroadcast.{CORBMod, CRBData}
import oss.giussi.cappio.impl.bcast.ReliableBroadcast.{RBData, RBMod}
import oss.giussi.cappio.impl.bcast.UniformReliableBroadcast.{URBData, URBMod}
import oss.giussi.cappio.impl.bcast.{BestEffortBroadcast, CausalOrderReliableBroadcast, ReliableBroadcast, UniformReliableBroadcast}
import oss.giussi.cappio.impl.net.PerfectLink
import oss.giussi.cappio.impl.net.PerfectLink.PLModule
import oss.giussi.cappio.impl.register.OneNRegularRegister._
import oss.giussi.cappio.impl.time.HeartbeatMsg
import shapeless.{:+:, CNil}

// TODO use cats
trait Show[A] {

  def show(a: A): String
}

object Show {

  import ShowSyntax._

  implicit val showString = new Show[String] {
    override def show(a: String): String = a
  }

  implicit val showUnit = new Show[Unit] {
    override def show(a: Unit): String = ""
  }

  implicit def showOption[P](show: Show[P]) = new Show[Option[P]] {
    override def show(a: Option[P]): String = ""
  }

  // Show requests

  implicit def showBEB[A](implicit e: Show[A]) = new Show[BebMod[A]#Req] {
    override def show(a: BestEffortBroadcast.BebBcast[A]): String = s"beb ${a.payload.msg}"
  }


  implicit def showRB[A](implicit e: Show[A]) = new Show[RBMod[A]#Req] {
    override def show(a: ReliableBroadcast.RBBcast[A]): String = s"rb ${a.payload.msg}"
  }


  implicit def showCORB[A](implicit e: Show[A]) = new Show[CORBMod[A]#Req] {
    override def show(a: CausalOrderReliableBroadcast.CRBBroadcast[A]): String = s"cb ${a.payload.msg}"
  }


  implicit def showURB[A](implicit e: Show[A]) = new Show[URBMod[A]#Req] {
    override def show(a: UniformReliableBroadcast.URBBcast[A]): String = s"urb ${a.payload.msg}"
  }

  implicit def showONRR[A](implicit e: Show[A]) = new Show[ONRRMod[A]#Req] {
    override def show(a: ONRRReq[A]): String = a match {
      case ONRRRead => "read"
      case ONRRWrite(v) => s"write $v"
    }
  }


  implicit def showPL[A](implicit e: Show[A]) = new Show[PLModule[A]#Req] {
    override def show(a: PerfectLink.PLSend[A]): String = s"send ${a.packet.payload}"
  }

  implicit def showSetOp = new Show[SetOp] {
    override def show(a: SetOp): String = a match {
      case AddOp(entry) => s"addOp($entry)"
      case RemoveOp(entry) => s"removeOp($entry)"
      case ClearOp => "clear"
    }
  }

  // Show payloads


  implicit def showRBData[A: Show] = new Show[RBData[A]] {
    override def show(a: RBData[A]): String = s"RBDATA[${a.msg.show}]"
  }

  implicit def showHeartbeat = new Show[HeartbeatMsg] {
    override def show(a: HeartbeatMsg): String = "heartbeat"
  }

  implicit def showComposedPayload[P1, P2](implicit show1: Show[P1], show2: Show[P2]) = new Show[P1 :+: P2 :+: CNil] {
    override def show(a: P1 :+: P2 :+: CNil): String = ???
  }

  implicit def showCORBData[P: Show] = new Show[CRBData[P]] {
    override def show(a: CRBData[P]): String = s"CRBDATA[${a.msg.show}]"
  }

  implicit def showURBData[P: Show] = new Show[URBData[P]] {
    override def show(a: URBData[P]): String = s"URBDATA[${a.msg.show}]"
  }

  implicit def showVectorTime = new Show[VectorTime] {
    override def show(a: VectorTime): String = s"[${a.value.toList.sortBy(_._1).map(_._2).mkString(",")}]"
  }

  implicit def showVersioned[P: Show] = new Show[Versioned[P]] {
    override def show(a: Versioned[P]): String = s"${a.value.show} [${a.vectorTimestamp.show}]"
  }


  implicit def showONRRPayload[A : Show] = new Show[ONOp[A]]{
    override def show(a: ONOp[A]): String = ???
  }

  implicit def showCRDTReq = new Show[CRDTMod#Req] {
    override def show(a: SetRequest): String = a match {
      case Add(p) => s"Add $p"
      case Remove(p) => s"Remove $p"
    }
  }

}

object ShowSyntax {

  implicit class ShowOps[A](val a: A) extends AnyVal {
    def show(implicit ev: Show[A]): String = ev.show(a)
  }

}
