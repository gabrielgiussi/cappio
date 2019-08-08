package oss.giussi.cappio.ui.levels

import com.raquo.laminar.api.L
import com.raquo.laminar.api.L._
import com.raquo.laminar.nodes.ReactiveHtmlElement
import org.scalajs.dom.raw.HTMLElement
import oss.giussi.cappio
import oss.giussi.cappio.Conditions.Condition
import oss.giussi.cappio.impl.net.FairLossLink.FLLSend
import oss.giussi.cappio.ui.ActionSelection.Inputs
import oss.giussi.cappio.ui.core._
import oss.giussi.cappio.ui.levels.bcast.{BEBLevel, URBLevel}
import oss.giussi.cappio.ui.{ActionSelection, Diagram}
import oss.giussi.cappio.{Mod => ModT, _}

object Levels {

  val RAW_LEVELS = List(
    Documentation("broadcast"),
    BEBLevel(4, 3),
    //URBLevel(4, 3)
  )

  val INDEXED_LEVELS: Map[LevelId, IndexedLevel] = RAW_LEVELS.zipWithIndex.map { case (level,index) => LevelId(index) -> IndexedLevel(index,level) }.toMap

  val LEVELS = INDEXED_LEVELS.values.toList

  val $approved = {
    val pendingLevels: Var[Set[LevelId]] = Var(INDEXED_LEVELS.keySet)

    pendingLevels.signal.foreach(println)(unsafeWindowOwner)

    INDEXED_LEVELS.map { case (id,level) => level.s.status.addObserver(Observer {
      _ => pendingLevels.update(_ - id)
    })(unsafeWindowOwner) }

    pendingLevels.signal.map(_.isEmpty)
  }

  $approved.foreach(x => if (x) println("Passed all") else println("pending"))(unsafeWindowOwner)
}


case class IndexedLevel(x: Int, s: Selection)

sealed trait Selection {
  def render: ReactiveHtmlElement[HTMLElement]

  def status: EventStream[LevelPassed.type]
}

case class Documentation(doc: String) extends Selection {
  override def render = div(cls := "container-fluid mt-5",
    div(cls := "row wow fadeIn",
      div(cls := "col-md mb-4",
        div(cls := "card",
          div(cls := "card-body",
            doc
          )
        )
      )
    )
  )

  override def status: EventStream[LevelPassed.type] = EventStream.fromValue(LevelPassed,true).map(x => { println("paso"); x })
}

trait Level extends Selection {

  val processes: Processes

  val $actions: Signal[List[Action]]

  def diagram: Div = Diagram(processes, $actions)

  def actionSelection: Div

  def states: Div

  val $conditions: Signal[List[ConditionResult]]

  def conditions: Div = {
    def renderCondition(id: Int, initial: ConditionResult, $changes: Signal[ConditionResult]) = li(cls := "list-group-item",
      label(
        child <-- $changes.map(_.result match {
          case Successful => s"${initial.description} ok"
          case Error(msg) => s"${initial.description} $msg"
        })
      )
    )
    div(cls := "card mb-4",
      div(cls := "card-body",
        ul(cls := "list-group",
          children <-- $conditions.splitIntoSignals(_.id)(renderCondition)
        )
      )
    )
  }

  final override def render = div(cls := "container-fluid mt-5",
    div(cls := "row wow fadeIn",
      div(cls := "col-md-9 mb-4",
        div(cls := "card",
          div(cls := "card-body",
            diagram
          )
        )
      ),
      div(cls := "col-md-3 mb-4",
        div(cls := "card mb-4",
          div(cls := "card-header text-center", "Action Selection"),
          div(cls := "card-body",
            actionSelection
          )
        ),
        conditions
      )
    ),
    div(
      states
    )
  )

}

case class RequestBatch[Req](requests: Map[ProcessId, ProcessInput[Req]]) {
  def add(id: ProcessId, req: ProcessInput[Req]): RequestBatch[Req] = copy(requests = requests + (id -> req))

  def add(id: ProcessId, req: Req): RequestBatch[Req] = add(id, cappio.ProcessRequest(id, req))

  def crash(id: ProcessId): RequestBatch[Req] = add(id, Crash(id)) // Crash(id) or just Crash?

  def remove(processId: ProcessId) = copy(requests = requests - processId)
}

sealed trait Op[M <: ModT]

case class NextReq[M <: ModT](value: RequestBatch[M#Req]) extends Op[M]

case class NextDeliver[M <: ModT](value: DeliverBatch[M#Payload]) extends Op[M]

// TODO porque no le gusta que sean case objects?
case class Prev[M <: ModT]() extends Op[M]

case class Reset[M <: ModT]() extends Op[M]

object Snapshot {

  final def toRequest[R](reqPayload: R => String)(p: ProcessId, req: ProcessInput[R], i: Index): Action = req match {
    case ProcessRequest(_, req) => Request(p, i, reqPayload(req))
    case Crash(_) => Crashed(p, i)
  }

  final def toIndication[Ind](indicationPayload: Ind => String)(i: Index)(ind: IndicationFrom[Ind]) = Indication(ind.p, i, indicationPayload(ind.i))

  def sendToUndelivered[P](index: Index)(send: FLLSend[P]): Undelivered = Undelivered(send.packet.from, send.packet.to, send.packet.id, send.packet.payload.toString, index)

  def next[M <: ModT](indicationPayload: M#Ind => String, reqPayload: M#Req => String)(snapshot: Snapshot[M], op: Op[M]): Snapshot[M] = (snapshot,op) match {
    case (c@Snapshot(current, actions, wr@WaitingRequest(_), _), NextReq(req)) =>
      val RequestResult(sent, ind, wd) = wr.request(req.requests.values.toSeq)
      val sends = sent.map(sendToUndelivered(current))
      val requests = req.requests.map { case (p, r) => toRequest(reqPayload)(p, r, current) }
      val indications = ind.map(toIndication(indicationPayload)(current))
      Snapshot(current.next, actions ++ indications ++ requests ++ sends, wd, Some(c))
    case (c@Snapshot(current, actions, wd@WaitingDeliver(_), _), NextDeliver(del)) =>
      val DeliverResult(sent, ind, wr) = wd.deliver(del)
      val sends = sent.map(sendToUndelivered(current))
      val delivers = del.ops.values.flatMap {
        case Left(FLLDeliver(Packet(id, payload, from, to, _))) => Some(Delivered(from, to, id, payload.toString, actions.collectFirst {
          case Undelivered(`from`, `to`, `id`, _, s) => s
        }.get, current)) // FIXME refactor code
        case _ => None
      }
      val drops = del.ops.values.flatMap {
        case Right(Drop(Packet(id, payload, from, to, instance))) => Some(Dropped(from,to,id,payload.toString,actions.collectFirst{
          case Undelivered(`from`, `to`, `id`, _, s) => s
        }.get, current))
        case _ => None
      }
      val indications = ind.map(toIndication(indicationPayload)(current))
      val filtered = {
        val deliversKeys = delivers.map(_.id).toSet
        val dropsKeys = drops.map(_.id).toSet
        actions.filter {
          case u: Undelivered if deliversKeys.contains(u.id) => false
          case u: Undelivered if dropsKeys.contains(u.id) => false
          case _ => true
        }
      }
      Snapshot(current.next, filtered ++ indications ++ delivers ++ sends ++ drops, wr, Some(c))
    case (Snapshot(_, _, _, Some(prev)), Prev()) => prev
    case (s, input) =>
      //org.scalajs.dom.console.log(s"%c Bad input $input for step ${s.step} ", "background: #222; color: #bada55")
      s
  }

}

case class Snapshot[M <: ModT](index: Index, actions: List[Action], step: Step[M], prev: Option[Snapshot[M]])

case class LevelId(x: Int)

sealed trait LevelResult

case object LevelPassed extends LevelResult

case object Pending extends LevelResult

abstract class AbstractLevel[M <: ModT](scheduler: Scheduler[M], conditions: List[Condition[Scheduler[M]]] = List.empty) extends Level {

  type Payload = M#Payload
  type State = M#State
  type Ind = M#Ind
  type Req = M#Req
  type ReqBatch = RequestBatch[Req]
  type DelBatch = DeliverBatch[M#Payload]

  val processes = Processes(scheduler.processes.keySet)

  case class ProcessState(id: ProcessId, state: State, status: ProcessStatus)

  val $next = new EventBus[Op[M]]

  def requestPayload(req: M#Req): String

  val indicationPayload: M#Ind => String

  val $snapshots: Signal[Snapshot[M]] = {
    val ns = Snapshot.next[M](indicationPayload, requestPayload) _
    $next.events.fold(Snapshot(Index(0), List.empty, WaitingRequest(TickScheduler(scheduler)), None))(ns)
  }

  val $steps: Signal[Step[M]] = $snapshots.map(_.step)

  override val $actions = $snapshots.map(_.actions) //.changes // TODO puedo devolver una signal directamente total al principio no va a tener actions

  val reqTypes: List[Inputs[Req]]

  override def actionSelection: Div = {
    div(
      div(
        child <-- $steps.map {
          case WaitingRequest(sch) => ActionSelection.reqBatchInput(reqTypes, sch.scheduler.availableProcesses.toList, $next.writer.contramap[RequestBatch[M#Req]](r => NextReq(r)))
          case WaitingDeliver(sch) => ActionSelection.networkInput(sch.scheduler.availablePackets, $next.writer.contramap[DelBatch](r => NextDeliver(r)))
        }
      ),
      div(
        button(
          cls := "btn btn-secondary",
          onClick.mapToValue(Prev[M]()) --> $next.writer,
          "Previous"
        )
      )
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

  override val $conditions = $snapshots.map(snap => conditions.map(_.apply(snap.step.scheduler.scheduler)))

  override def status: EventStream[LevelPassed.type] = $conditions.changes.filter(_.find(!_.ok).isEmpty).map(_ => LevelPassed)

}