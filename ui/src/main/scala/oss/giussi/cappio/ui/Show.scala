package oss.giussi.cappio.ui

import oss.giussi.cappio.impl.bcast.BestEffortBroadcast.BebMod
import oss.giussi.cappio.impl.bcast.CausalOrderReliableBroadcast.{CORBMod, CRBData}
import oss.giussi.cappio.impl.bcast.ReliableBroadcast.{RBData, RBMod}
import oss.giussi.cappio.impl.bcast.{BestEffortBroadcast, CausalOrderReliableBroadcast, ReliableBroadcast, UniformReliableBroadcast}
import oss.giussi.cappio.impl.bcast.UniformReliableBroadcast.{URBData, URBMod}
import oss.giussi.cappio.impl.register.OneNRegularRegister
import oss.giussi.cappio.impl.register.OneNRegularRegister.{ONACK, ONREAD, ONRRMod, ONRRRead, ONRRWrite, ONVALUE, ONWRITE}
import oss.giussi.cappio.impl.time.PerfectFailureDetector
import shapeless.{:+:, CNil, Inl, Inr}

// TODO use cats
trait Show[A] {

  def show(a: A): String
}

object Show {

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

  // Show payloads

  implicit def showRBPayload[A](implicit e: Show[A]) = new Show[RBMod[A]#Payload] {
    override def show(a: :+:[PerfectFailureDetector.HeartbeatMsg, RBData[A] :+: CNil]): String = a match {
      case Inr(Inl(RBData(_, msg))) => s"RBDATA[${e.show(msg)}]"
      case _ => "hearbeat"
    }
  }

  implicit def showURBPayload[A](implicit e: Show[A]) = new Show[URBMod[A]#Payload] {
    override def show(a: :+:[PerfectFailureDetector.HeartbeatMsg, UniformReliableBroadcast.URBData[A] :+: CNil]): String = a match {
      case Inr(Inl(URBData(_, msg))) => s"URBDATA[${e.show(msg)}]"
      case _ => "hearbeat"
    }
  }

  implicit def showCORBPayload[A](implicit e: Show[A]) = new Show[CORBMod[A]#Payload] {
    override def show(a: :+:[PerfectFailureDetector.HeartbeatMsg, ReliableBroadcast.RBData[CausalOrderReliableBroadcast.CRBData[A]] :+: CNil]): String = a match {
      case Inr(Inl(RBData(sender, CRBData(mpast, msg)))) => s"CRBDATA[${e.show(msg)}]" // TODO show mpast?
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
}

object ShowSyntax {

  implicit class ShowOps[A](val a: A) extends AnyVal {
    def show(implicit ev: Show[A]): String = ev.show(a)
  }

}
