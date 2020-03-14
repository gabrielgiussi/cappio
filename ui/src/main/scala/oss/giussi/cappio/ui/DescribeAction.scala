package oss.giussi.cappio.ui

import oss.giussi.cappio.crdt.Versioned
import oss.giussi.cappio.crdt.pure.impl.{AddOp, ClearOp, RemoveOp, SetOp}
import oss.giussi.cappio.impl.CRDTApp.{Add, Remove, SetRequest}
import oss.giussi.cappio.impl.PhotosApp.{AlbumOpResult, AlbumOp}
import oss.giussi.cappio.impl.bcast.BestEffortBroadcast
import oss.giussi.cappio.impl.bcast.BestEffortBroadcast.BebMod
import oss.giussi.cappio.impl.bcast.CausalOrderReliableBroadcast.{CRBBroadcast, CRBData, CRBDeliver}
import oss.giussi.cappio.impl.bcast.ReliableBroadcast.{RBBcast, RBData, RBDeliver}
import oss.giussi.cappio.impl.bcast.UniformReliableBroadcast.{URBBcast, URBData, URBDeliver}
import oss.giussi.cappio.impl.bcast.WaitingCausalBroadcast.WCDeliver
import oss.giussi.cappio.impl.net.PerfectLink.{PLDeliver, PLSend}
import oss.giussi.cappio.impl.time.HeartbeatMsg
import oss.giussi.cappio.ui.core.{ActionTag, BCAST, HEARTBEAT}
import shapeless.{:+:, CNil, Inl, Inr}


object ActionDescription {

  def withName(name: String, payload: String, tags: Set[ActionTag] = Set.empty): ActionDescription = new ActionDescription(Some(name), payload, tags)

  // OR create PayloadDescription?
  def withoutName(payload: String, tags: Set[ActionTag] = Set.empty): ActionDescription = new ActionDescription(None, payload, tags)
}

case class ActionDescription(name: Option[String], payload: String, tags: Set[ActionTag])

trait DescribeAction[A] {

  def describe(action: A): ActionDescription
}

object DescribeAction {


  import ShowSyntax._
  import DescribeActionSyntax._

  implicit def describeBcast[P: Show] = new DescribeAction[BebMod[P]#Req] {
    override def describe(action: BestEffortBroadcast.BebBcast[P]): ActionDescription = ActionDescription.withName("beb-bcast", action.payload.msg.show)
  }

  implicit def describeBdeliver[P: Show] = new DescribeAction[BebMod[P]#Ind] {
    override def describe(action: BestEffortBroadcast.BebDeliver[P]): ActionDescription = ActionDescription.withName("beb-deliver", action.payload.msg.show)
  }

  implicit val describeString = new DescribeAction[String] {
    override def describe(action: String): ActionDescription = ActionDescription.withoutName(action)
  }


  implicit def describeHeartbeat = new DescribeAction[HeartbeatMsg] {
    override def describe(action: HeartbeatMsg): ActionDescription = ActionDescription.withoutName("heartbeat", Set(HEARTBEAT))
  }

  implicit def describeRBData[P: DescribeAction] = new DescribeAction[RBData[P]] {
    override def describe(action: RBData[P]): ActionDescription = {
      val ActionDescription(_, payload, tags) = action.msg.describe
      ActionDescription.withoutName(s"RBDATA[$payload]", tags + BCAST)
    }
  }

  implicit def describeCRBData[P: DescribeAction] = new DescribeAction[CRBData[P]] {
    override def describe(action: CRBData[P]): ActionDescription = {
      val ActionDescription(_, payload, tags) = action.msg.describe
      ActionDescription.withoutName(s"CRBDATA[$payload]", tags + BCAST) // TODO include past!
    }
  }


  implicit def describeURBData[P: DescribeAction] = new DescribeAction[URBData[P]] {
    override def describe(action: URBData[P]): ActionDescription = {
      val ActionDescription(_, payload, tags) = action.msg.describe
      ActionDescription.withoutName(s"URBDATA[payload=$payload,sender=${action.sender}]", tags + BCAST)
    }
  }

  implicit def describeSetOp = new DescribeAction[SetOp] {
    override def describe(action: SetOp): ActionDescription = {
      val payload = action match {
        case AddOp(entry) => s"add($entry)"
        case RemoveOp(entry) => s"remove($entry)"
        case ClearOp => s"clear"
      }
      ActionDescription.withoutName(payload)
    }
  }

  implicit def describeVersioned[P: DescribeAction] = new DescribeAction[Versioned[P]] {
    override def describe(action: Versioned[P]): ActionDescription = {
      val vectorClock = s"(${action.vectorTimestamp.value.toList.sortBy(_._1).map(_._2).mkString(",")})" // TODO where should this go?
      val ActionDescription(_, payload, tags) = action.value.describe
      ActionDescription.withoutName(s"$payload-$vectorClock", tags)
    }
  }


  implicit def describeComposePayload[P1, P2](implicit describeP1: DescribeAction[P1], describeP2: DescribeAction[P2]) = new DescribeAction[P1 :+: P2 :+: CNil] {
    override def describe(action: P1 :+: P2 :+: CNil): ActionDescription = action match {
      case Inl(p1) => p1.describe
      case Inr(Inl(p2)) => p2.describe
      case _ => null
    }
  }

  implicit def describeDemoReq[P: Show] = new DescribeAction[PLSend[P]] {
    override def describe(action: PLSend[P]): ActionDescription = ActionDescription.withName("send", action.packet.payload.show)
  }

  implicit def describeDemoInd[P: Show] = new DescribeAction[PLDeliver[P]] {
    override def describe(action: PLDeliver[P]): ActionDescription = ActionDescription.withName("deliver", action.packet.payload.show)
  }

  implicit def describeRBReq[P: Show] = new DescribeAction[RBBcast[P]] {
    override def describe(action: RBBcast[P]): ActionDescription = ActionDescription.withName("rb-bcast", action.show)
  }

  implicit def describeRBInd[P: Show] = new DescribeAction[RBDeliver[P]] {
    override def describe(action: RBDeliver[P]): ActionDescription = ActionDescription.withName("rb-deliver", action.payload.msg.show)
  }


  implicit def describeURBReq[P: Show] = new DescribeAction[URBBcast[P]] {
    override def describe(action: URBBcast[P]): ActionDescription = ActionDescription.withName("urb-bcast", action.payload.msg.show)
  }

  implicit def describeURBInd[P: Show] = new DescribeAction[URBDeliver[P]] {
    override def describe(action: URBDeliver[P]): ActionDescription = ActionDescription.withName("urb-deliver", action.payload.show)
  }

  implicit def describeCRDTRequest = new DescribeAction[SetRequest] {
    override def describe(action: SetRequest): ActionDescription = action match {
      case Add(p) => ActionDescription.withName("add", p)
      case Remove(p) => ActionDescription.withName("remove", p)
    }
  }

  implicit def describeCRDTDeliver[P: Show] = new DescribeAction[WCDeliver[P]] {
    override def describe(action: WCDeliver[P]): ActionDescription = ActionDescription.withName("updated", action.msg.show)
  }


  implicit def describeCausalBcast[P: Show] = new DescribeAction[CRBBroadcast[P]] {
    override def describe(action: CRBBroadcast[P]): ActionDescription = ActionDescription.withName("crb-bcast", action.payload.msg.show)
  }

  implicit def describeCausalDeliver[P: Show] = new DescribeAction[CRBDeliver[P]] {
    override def describe(action: CRBDeliver[P]): ActionDescription = ActionDescription.withName("crb-deliver", action.msg.show)
  }

  implicit val describeAlbumOp = new DescribeAction[AlbumOp] {
    override def describe(action: AlbumOp): ActionDescription = ActionDescription(Some("album-op"),"",Set())
  }

  implicit val describeAlbumResult = new DescribeAction[AlbumOpResult] {
    override def describe(action: AlbumOpResult): ActionDescription = ActionDescription.withName("album-op-res","")
  }
}

object DescribeActionSyntax {

  implicit class DescribeOps[A](val action: A) extends AnyVal {
    def describe(implicit ev: DescribeAction[A]): ActionDescription = ev.describe(action)
  }

}
