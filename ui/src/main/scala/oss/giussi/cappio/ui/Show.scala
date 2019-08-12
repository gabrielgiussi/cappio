package oss.giussi.cappio.ui

import oss.giussi.cappio.impl.bcast.ReliableBroadcast.{RBData, RBMod}
import oss.giussi.cappio.impl.bcast.UniformReliableBroadcast
import oss.giussi.cappio.impl.bcast.UniformReliableBroadcast.{URBData, URBMod}
import oss.giussi.cappio.impl.register.OneNRegularRegister
import oss.giussi.cappio.impl.register.OneNRegularRegister.{ONACK, ONREAD, ONRRMod, ONVALUE, ONWRITE}
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

  implicit def showRB[A](implicit e: Show[A]) = new Show[RBMod[A]#Payload] {
    override def show(a: :+:[PerfectFailureDetector.HeartbeatMsg, RBData[A] :+: CNil]): String = a match {
      case Inr(Inl(RBData(_, msg))) => s"RBDATA[${e.show(msg)}]"
      case _ => "hearbeat"
    }
  }

  implicit def showURB[A](implicit e: Show[A]) = new Show[URBMod[A]#Payload] {
    override def show(a: :+:[PerfectFailureDetector.HeartbeatMsg, UniformReliableBroadcast.URBData[A] :+: CNil]): String = a match {
      case Inr(Inl(URBData(_, msg))) => s"RBDATA[${e.show(msg)}]"
      case _ => "hearbeat"
    }
  }

  implicit def showONRR[A : Show] = new Show[ONRRMod[A]#Payload]{
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
