package oss.giussi.cappio.ui.levels

import com.raquo.laminar.api.L._
import com.raquo.laminar.nodes.ReactiveHtmlElement
import oss.giussi.cappio._
import oss.giussi.cappio.impl.net.FairLossLink.FLLSend
import oss.giussi.cappio.ui.core._
import oss.giussi.cappio.ui.levels.bcast.{BEBLevel, URBLevel}
import oss.giussi.cappio.ui.{ActionSelection, Diagram}

object Levels {


  val levels: List[IndexedLevel] = List(
    IndexedLevel(1, () => BEBLevel(4,3)),
    IndexedLevel(2, () => URBLevel(4,3))
  )

}


// FIXME createLevel es por ahora para que siempre genere un nuevo nivel porque las signal ya van a haber cambiado el valor initial
case class IndexedLevel(x: Int, createLevel: () => Level)

trait Level {

  val processes: Processes

  val $actions: EventStream[List[Action]]

  def diagram: Div = Diagram(processes, $actions)

  def actionSelection: Div

  def states: Div

  def conditions: Div

}

abstract class AbstractLevel[R, S, I, ReqType](scheduler: Scheduler[R, S, I]) extends Level {

  val processes = Processes(scheduler.processes.keySet)

  case class ProcessState(id: ProcessId, state: S, status: ProcessStatus)

  val $next = new EventBus[Either[RequestBatch[R], DeliverBatch]]

  val $actionsBus = new EventBus[List[Action]]

  case class Snapshot(index: Int, actions: List[Action], step: Step[R, S, I])

  def requestPayload(req: R): String

  def indicationPayload(ind: I): String

  final def toRequest(p: ProcessId, req: R, i: Index) = Request(p,i,requestPayload(req))

  final def toIndication(ind: IndicationFrom[I], i: Index) = Indication(ind.p,i,indicationPayload(ind.i))

  val $snapshots = $next.events.fold[Snapshot](Snapshot(0, List.empty, WaitingRequest(TickScheduler(scheduler)))) {
    // TODO remove duplicated code
    case (Snapshot(current, actions, wr@WaitingRequest(_)), Left(req)) =>
      val index = Index(current)
      val (sent, ind, wd) = wr.request(req)
      val sends = sent.map { case FLLSend(Packet(id, payload, from, to, _)) => Undelivered(from, to, id, payload.toString, index) }
      val requests = req.requests.map { case (p,r) => toRequest(p, r, index) }
      val indications = ind.map(toIndication(_, Index(current)))
      Snapshot(current + 1, actions ++ indications ++ requests ++ sends, wd)
    case (Snapshot(current, actions, wd@WaitingDeliver(_)), Right(del)) =>
      val index = Index(current)
      val (sent, ind, wr) = wd.deliver(del)
      val sends = sent.map { case FLLSend(Packet(id, payload, from, to, _)) => Undelivered(from, to, id, payload.toString, index) }
      val delivers = del.ops.values.flatMap {
        case Left(FLLDeliver(Packet(id, payload, from, to, _))) => Some(Delivered(from, to, id, payload.toString, actions.collect {
          case Undelivered(_, _, `id`, _, s) => s
        }.head, index)) // FIXME esto es solo para pruebas porque es un asco
        case _ => None
      }
      val indications = ind.map(toIndication(_, index))
      val filtered = actions.filter {
        case Undelivered(_, _, id, _, _) if delivers.map(_.uuid).toList.contains(id) => false
        case _ => true
      }
      Snapshot(current + 1, filtered ++ indications ++ delivers ++ sends, wr)
    case (s, _) => s // TODO log error in console
  }
  val $steps = $snapshots.map(_.step)

  override val $actions = $snapshots.map(_.actions).changes // TODO puedo devolver una signal directamente total al principio no va a tener actions

  // FIXME cada vez que se seleccione un processId se va a repintar, no quiero eso!
  def actionSel(obs: Observer[Option[R]])(processId: ProcessId): ReqType => ReactiveHtmlElement[org.scalajs.dom.html.Element]

  val reqTypes: List[ReqType]

  override def actionSelection: Div = {
    div(
      child <-- $steps.map {
        case WaitingRequest(_) => ActionSelection.reqBatchInput(reqTypes, processes, $next.writer.contramap[RequestBatch[R]](r => Left(r)), actionSel)
        case WaitingDeliver(sch) => ActionSelection.networkInput(sch.scheduler.network.inTransit(), $next.writer.contramap(r => Right(r)))
      }
    )
  }

  def renderState(id: ProcessId, initial: ProcessState, $states: Signal[ProcessState]) = div(
    cls := "col-md-4 mb-4",
    div(
      cls := "card",
      div(
        cls := "card-body",
        renderStateI(id, initial, $states)
      )
    )
  )

  // TODO make it optional (beb doesn't have state)
  def renderStateI(id: ProcessId, initial: ProcessState, $states: Signal[ProcessState]): Modifier[Div] = label(id.toString)

  override def states: Div = div(
    cls := "row wow fadeIn",
    // children <-- $states.split(_.id)(renderState) TODO
  )

  override def conditions: Div = div()


}