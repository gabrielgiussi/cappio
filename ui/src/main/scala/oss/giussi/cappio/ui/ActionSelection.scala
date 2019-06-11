package oss.giussi.cappio.ui

import com.raquo.airstream.core.Observable
import com.raquo.laminar.api.L._
import oss.giussi.cappio.ProcessId
import oss.giussi.cappio.impl.bcast.BestEffortBroadcast.BebBcast
import oss.giussi.cappio.impl.bcast.UniformReliableBroadcast.Payload

object ActionSelection {

  // TODO
  def requests(requests: Set[String]) = {
    val streams = new EventBus[Int]()
    div(

    )
  }

  // requires an input of
  def deliver() = {

  }

  def crashInput() = {

  }

  def processSelection(processes: List[ProcessId], selection: WriteBus[Option[ProcessId]]) = {
    select(
      cls := "browser-default custom-select mb-4",
      inContext(thisNode => onInput.mapTo(thisNode.ref.value).map(v => Some(v).filterNot(_ == "unset").map(_.toInt).map(ProcessId)) --> selection),
      option(
        "Select a process",
        value := "unset"
      ) :: processes.map(p => option(
        p.id.toString,
        value := p.id.toString,
      ))
    )
  }

  def bebBroadcast(processes: List[ProcessId], beb: WriteBus[(ProcessId,BebBcast)]) = {
    val process = new EventBus[Option[ProcessId]]
    val payload = new EventBus[Option[String]]
    val comb = process.events.combineWith(payload.events)
      .map{ case (o1,o2) => for {
        pid <- o1
        pay <- o2
      } yield (pid,BebBcast(Payload(pay),null)) }.filter(_.isDefined).map(_.get)

    val elem = div(
      "BebBcast",
      processSelection(processes,process.writer),
      input(
        inContext(thisNode => onChange.mapTo(thisNode.ref.value).map(v => Option(v).filterNot(_.isEmpty)) --> payload)
      )
    )
    beb.addSource(comb)(elem)
    elem
  }


}
