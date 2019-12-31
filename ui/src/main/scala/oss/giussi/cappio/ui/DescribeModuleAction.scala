package oss.giussi.cappio.ui

import oss.giussi.cappio.Mod

object DescribeModuleAction {
  import DescribeActionSyntax._

  implicit def describeModule[M <: Mod](implicit descReq: DescribeAction[M#Req], descInd: DescribeAction[M#Ind], descPayload: DescribeAction[M#Payload]): DescribeModuleAction[M] = new DescribeModuleAction[M] {
    override def describeReq(action: M#Req): ActionDescription = action.describe

    override def describeInd(action: M#Ind): ActionDescription = action.describe

    override def describePayload(action: M#Payload): ActionDescription = action.describe
  }
}

trait DescribeModuleAction[M <: Mod]{

  def describeReq(action: M#Req): ActionDescription

  def describeInd(action: M#Ind): ActionDescription

  def describePayload(action: M#Payload): ActionDescription

  final def reqPay(action: M#Req): (String, String) = {
    val desc = describeReq(action)
    (desc.name.getOrElse(""), desc.payload) // FIXME
  }

  final def indPay(action: M#Ind): String = describeInd(action).payload

  final def payload(action: M#Payload): String = describePayload(action).payload

}