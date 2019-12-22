package oss.giussi.cappio.ui

import oss.giussi.cappio.crdt.{VectorTime, Versioned}
import oss.giussi.cappio.crdt.pure.impl.{AddOp, ClearOp, RemoveOp, SetOp}
import oss.giussi.cappio.impl.CRDTApp
import oss.giussi.cappio.impl.CRDTApp.CRDTMod
import oss.giussi.cappio.impl.bcast.BestEffortBroadcast.BebMod
import oss.giussi.cappio.impl.bcast.CausalOrderReliableBroadcast.{CORBMod, CRBData}
import oss.giussi.cappio.impl.bcast.ReliableBroadcast.{RBData, RBMod}
import oss.giussi.cappio.impl.bcast.{BestEffortBroadcast, CausalOrderReliableBroadcast, ReliableBroadcast, UniformReliableBroadcast}
import oss.giussi.cappio.impl.bcast.UniformReliableBroadcast.{URBData, URBMod}
import oss.giussi.cappio.impl.bcast.WaitingCausalBroadcast.WCBMod
import oss.giussi.cappio.impl.net.PerfectLink
import oss.giussi.cappio.impl.net.PerfectLink.PLModule
import oss.giussi.cappio.impl.register.OneNRegularRegister
import oss.giussi.cappio.impl.register.OneNRegularRegister.{ONACK, ONREAD, ONRRMod, ONRRRead, ONRRWrite, ONVALUE, ONWRITE}
import oss.giussi.cappio.impl.time.PerfectFailureDetector
import oss.giussi.cappio.impl.time.PerfectFailureDetector.HeartbeatMsg
import shapeless.{:+:, CNil, Inl, Inr}

// TODO use cats
trait Show[A] {

  def show(a: A): String
}

object Show {

  import ShowSyntax._

  implicit val showString = new Show[String] {
    override def show(a: String): String = a
  }

  // Show requests

  implicit def showBEB[A](implicit e: Show[A]) = new Show[BebMod[A]#Req] {
    override def show(a: BestEffortBroadcast.BebBcast[A]): String = s"beb ${a.payload.msg}"
  }

  implicit def showRB[A](implicit e: Show[A]) = new Show[RBMod[A]#Req] {
    override def show(a: ReliableBroadcast.RBBcast[A]): String = s"rb ${a.payload.msg}"
  }

  implicit def showURB[A](implicit e: Show[A]) = new Show[URBMod[A]#Req] {
    override def show(a: UniformReliableBroadcast.URBBcast[A]): String = s"urb ${a.payload.msg}"
  }

  implicit def showCORB[A](implicit e: Show[A]) = new Show[CORBMod[A]#Req] {
    override def show(a: CausalOrderReliableBroadcast.CRBBroadcast[A]): String = s"cb ${a.payload.msg}"
  }

  implicit def showONRR[A](implicit e: Show[A]) = new Show[ONRRMod[A]#Req] {
    override def show(a: OneNRegularRegister.ONRRReq[A]): String = a match {
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

  implicit def showRBData[A : Show] = new Show[RBData[A]] {
    override def show(a: RBData[A]): String = s"RBDATA[${a.msg.show}]"
  }

  implicit def showRBPayload[A : Show] = new Show[RBMod[A]#Payload] {
    override def show(a: :+:[PerfectFailureDetector.HeartbeatMsg, RBData[A] :+: CNil]): String = a match {
      case Inr(Inl(rbdata)) => rbdata.show
      case _ => "hearbeat"
    }
  }

  implicit def showURBPayload[A : Show] = new Show[URBMod[A]#Payload] {
    override def show(a: :+:[PerfectFailureDetector.HeartbeatMsg, UniformReliableBroadcast.URBData[A] :+: CNil]): String = a match {
      case Inr(Inl(URBData(_, msg))) => s"URBDATA[${msg.show}]"
      case _ => "hearbeat"
    }
  }

  implicit def showCORBPayload[A : Show] = new Show[CORBMod[A]#Payload] {
    override def show(a: :+:[PerfectFailureDetector.HeartbeatMsg, ReliableBroadcast.RBData[CausalOrderReliableBroadcast.CRBData[A]] :+: CNil]): String = a match {
      // TODO show mpast?
      // TODO use show of rbdata? or there are to many things in the UI?
      case Inr(Inl(RBData(sender, CRBData(mpast, msg)))) => s"CRBDATA[${msg.show}]"
      case _ => "heartbeat"
    }
  }

  implicit def showONRRPayload[A : Show] = new Show[ONRRMod[A]#Payload]{
    override def show(a: :+:[OneNRegularRegister.ONOp[A], OneNRegularRegister.ONDirect[A] :+: CNil]): String = a match {
      case Inl(ONREAD(rid)) => s"read $rid"
      case Inl(ONWRITE(timestamp, v)) => s"write ($timestamp,$v)"
      case Inr(Inl(ONACK(timestamp))) => s"ack $timestamp"
      case Inr(Inl(ONVALUE(r, ts, v))) => s"value ($r,$ts,$v)"
      case _ => ""
    }
  }

  implicit def showHeartbeat = new Show[HeartbeatMsg] {
    override def show(a: HeartbeatMsg): String = "heartbeat"
  }

  implicit def showVectorTime = new Show[VectorTime] {
    override def show(a: VectorTime): String = s"[${a.value.toList.sortBy(_._1).map(_._2).mkString(",")}]"
  }

  implicit def showVersioned[P : Show] = new Show[Versioned[P]] {
    override def show(a: Versioned[P]): String = s"${a.value.show} [${a.vectorTimestamp.show}]"
  }

  implicit def showWCBPayload[A : Show] = new Show[WCBMod[A]#Payload]{
    override def show(a: :+:[PerfectFailureDetector.HeartbeatMsg, ReliableBroadcast.RBData[Versioned[A]] :+: CNil]): String = a match {
      case Inl(hbeat) => hbeat.show
      case Inr(Inl(rbdata)) => rbdata.show
      case _ => ""
    }
  }

  implicit def showCRDTReq = new Show[CRDTMod#Req]{
    import CRDTApp._
    override def show(a: CRDTApp.SetRequest): String = a match {
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
