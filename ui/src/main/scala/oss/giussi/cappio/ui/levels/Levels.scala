package oss.giussi.cappio.ui.levels

import com.raquo.laminar.api.L._
import com.raquo.laminar.nodes.ReactiveHtmlElement
import oss.giussi.cappio
import oss.giussi.cappio._
import oss.giussi.cappio.impl.net.FairLossLink.FLLSend
import oss.giussi.cappio.ui.core._
import oss.giussi.cappio.ui.levels.bcast.{BEBLevel, URBLevel}
import oss.giussi.cappio.ui.{ActionSelection, Diagram}
import oss.giussi.cappio.{Mod => ModT}

object Levels {


  val INDEXED_LEVELS: Map[Int,IndexedLevel] = Map(
    1 -> IndexedLevel(1, () => BEBLevel(4,3)),
    2 -> IndexedLevel(2, () => URBLevel(4,3))
  )

  val LEVELS = INDEXED_LEVELS.values.toList

}


// FIXME createLevel es por ahora para que siempre genere un nuevo nivel porque las signal ya van a haber cambiado el valor initial
// remove indexed level?
case class IndexedLevel(x: Int, createLevel: () => Level)

trait Level {

  val processes: Processes

  val $actions: EventStream[List[Action]]

  def diagram: Div = Diagram(processes, $actions)

  def actionSelection: Div

  def states: Div

  def conditions: Div

}

case class RequestBatch[Req](requests: Map[ProcessId,ProcessInput[Req]]) {
  def add(id: ProcessId, req: ProcessInput[Req]): RequestBatch[Req] = copy(requests = requests + (id -> req))

  def add(id: ProcessId, req: Req): RequestBatch[Req] = add(id,cappio.ProcessRequest(id, req))

  def crash(id: ProcessId): RequestBatch[Req] = add(id,Crash(id)) // Crash(id) or just Crash?

  def remove(processId: ProcessId) = copy(requests = requests - processId)
}

// TODO review ReqType
abstract class AbstractLevel[M <: ModT, ReqType](scheduler: Scheduler[M]) extends Level {

  type Payload = M#Payload
  type State = M#State
  type Ind = M#Ind
  type Req = M#Req
  type ReqBatch = RequestBatch[Req]
  type DelBatch = DeliverBatch[M#Payload]

  val processes = Processes(scheduler.processes.keySet)

  case class ProcessState(id: ProcessId, state: State, status: ProcessStatus)

  sealed trait Op
  case class NextReq(value: ReqBatch) extends Op
  case class NextDeliver(value: DelBatch) extends Op
  case object Prev extends Op

  val $next = new EventBus[Op]

  val $actionsBus = new EventBus[List[Action]]

  case class Snapshot(index: Index, actions: List[Action], step: Step[M], prev: Option[Snapshot])

  def requestPayload(req: Req): String

  def indicationPayload(ind: Ind): String

  final def toRequest(p: ProcessId, req: ProcessInput[Req], i: Index): Action = req match {
    case ProcessRequest(_,req) => Request(p,i,requestPayload(req))
    case Crash(_) => Crashed(p,i)
  }

  final def toIndication(i: Index)(ind: IndicationFrom[Ind]) = Indication(ind.p,i,indicationPayload(ind.i))

  val $snapshots = {
    def sendToUndelivered[P](index: Index)(send: FLLSend[P]): Undelivered = Undelivered(send.packet.from, send.packet.to, send.packet.id, send.packet.payload.toString, index)

    val initialSnapshot = Snapshot(Index(0), List.empty, WaitingRequest(TickScheduler(scheduler)),None)
    $next.events.fold[Snapshot](initialSnapshot) {
      // TODO remove duplicated code
      case (c@Snapshot(current, actions, wr@WaitingRequest(_),_), NextReq(req)) =>
        val RequestResult(sent, ind, wd) = wr.request(req.requests.values.toSeq)
        val sends = sent.map(sendToUndelivered(current))
        val requests = req.requests.map { case (p, r) => toRequest(p, r, current) }
        val indications = ind.map(toIndication(current))
        Snapshot(current.next, actions ++ indications ++ requests ++ sends, wd, Some(c))
      case (c@Snapshot(current, actions, wd@WaitingDeliver(_),_), NextDeliver(del)) =>
        val DeliverResult(sent, ind, wr) = wd.deliver(del)
        val sends = sent.map(sendToUndelivered(current))
        val delivers = del.ops.values.flatMap {
          case Left(FLLDeliver(Packet(id, payload, from, to, _))) => Some(Delivered(from, to, id, payload.toString, actions.collect {
            case Undelivered(_, _, `id`, _, s) => s
          }.head, current)) // FIXME esto es solo para pruebas porque es un asco
          case _ => None
        }
        val indications = ind.map(toIndication(current))
        val filtered = actions.filter {
          case Undelivered(_, _, id, _, _) if delivers.map(_.uuid).toList.contains(id) => false
          case _ => true
        }
        Snapshot(current.next, filtered ++ indications ++ delivers ++ sends, wr, Some(c))
      case (Snapshot(_,_,_,Some(prev)),Prev) => prev
      case (s, input) =>
        org.scalajs.dom.console.log(s"%c Bad input ${input} for step ${s.step} ", "background: #222; color: #bada55")
        s
    }
  }

  val $steps = $snapshots.map(_.step)

  override val $actions = $snapshots.map(_.actions).changes // TODO puedo devolver una signal directamente total al principio no va a tener actions

  // FIXME cada vez que se seleccione un processId se va a repintar, no quiero eso!
  def actionSel(obs: Observer[Option[Req]])(processId: ProcessId): ReqType => ReactiveHtmlElement[org.scalajs.dom.html.Element]

  val reqTypes: List[ReqType]

  override def actionSelection: Div = {
    div(
      button( // TODO move
        onClick.mapToValue(Prev) --> $next.writer,
        "Previous"
      ),
      child <-- $steps.map {
        case WaitingRequest(_) => ActionSelection.reqBatchInput(reqTypes, processes, $next.writer.contramap[ReqBatch](r => NextReq(r)), actionSel)
        case WaitingDeliver(sch) => ActionSelection.networkInput(sch.scheduler.network.inTransit, $next.writer.contramap[DelBatch](r => NextDeliver(r)))
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