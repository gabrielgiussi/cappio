package oss.giussi.cappio.ui.levels

import com.raquo.laminar.api.L._
import com.raquo.laminar.nodes.ReactiveHtmlElement
import org.scalajs.dom.raw.HTMLElement
import oss.giussi.cappio
import oss.giussi.cappio.Conditions.Condition
import oss.giussi.cappio.impl.net.FairLossLink.FLLSend
import oss.giussi.cappio.ui.ActionSelection.Inputs
import oss.giussi.cappio.ui.core._
import oss.giussi.cappio.ui.levels.Snapshot.Conditions
import oss.giussi.cappio.ui.levels.bcast.{BEBLevel, CausalLevel, RBLevel, URBLevel}
import oss.giussi.cappio.ui.{ActionSelection, Diagram, Show}
import oss.giussi.cappio.{Mod => ModT, _}

object Levels {

  val RAW_LEVELS: List[LevelId => Selection] = List(
    Documentation("broadcast") _,
    _ => BEBLevel.simple(4, 3),
    _ => BEBLevel.broken(4, 3),
    _ => RBLevel(4,3),
    _ => URBLevel(4, 3),
    _ => CausalLevel.good(4,3)
    //_ => ONRRLevel(4, 6)
  )

  val INDEXED_LEVELS: Map[LevelId, IndexedLevel] = RAW_LEVELS.zipWithIndex.map { case (level, index) =>
    val levelId = LevelId(index)
    levelId -> IndexedLevel(index, level(levelId))
  }.toMap

  val LEVELS = INDEXED_LEVELS.values.toList.sortBy(_.x)

  val $pendingLevels: StrictSignal[Map[LevelId, LevelResult]] = {
    val pendingLevels: Var[Map[LevelId, LevelResult]] = Var(INDEXED_LEVELS.map{ case (k,_) => k -> Pending })

    INDEXED_LEVELS.map { case (id, level) => level.s.status.addObserver(Observer {
      _ => pendingLevels.update(_.updated(id, LevelPassed))
    })(unsafeWindowOwner)
    }

    pendingLevels.signal
  }

  val $approved = $pendingLevels.signal.map(_.forall(_._2 == LevelPassed))
}

// TODO shouldn't be LevelId instead X?
case class IndexedLevel(x: Int, s: Selection)

sealed trait Selection {
  def render: ReactiveHtmlElement[HTMLElement]

  def status: EventStream[LevelPassed.type]
}

case class Documentation(doc: String)(id: LevelId) extends Selection {
  override def render = div(cls := "container-fluid mt-5",
    div(cls := "row wow fadeIn",
      div(cls := "col-md mb-4",
        div(cls := "card",
          div(cls := "card-body",
            doc
          ),
          a(href := s"#${id.next.x}",
            "Next"
          )
        )
      )
    )
  )

  override def status: EventStream[LevelPassed.type] = EventStream.fromValue(LevelPassed, true)
}

case class ConditionLevel(id: Int, result: ConditionResult) {
  def ok = result.ok
}

case class LastSnapshot(current: Index, actions: List[Action])

trait Level extends Selection {

  val processes: Processes

  val $actions: Signal[LastSnapshot]

  def diagram: Div = Diagram(processes, $actions)

  def actionSelection: Div

  def states: Div

  val $conditions: Signal[List[ConditionLevel]]

  def conditions: Div = {
    def renderCondition(id: Int, initial: ConditionLevel, $changes: Signal[ConditionLevel]) = li(cls := "list-group-item",
      child <-- $changes.map(_.result.result match {
        case Successful => label(s"${initial.result.short}", span(cls := "badge badge-pill badge-success ml-3", i(cls := "fas fa-check")))
        case Error(msg) => label(s"$msg", span(cls := "badge badge-pill badge-danger ml-3", i(cls := "fas fa-exclamation")))
      })
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

  def init[M <: oss.giussi.cappio.Mod](step: Step[M]): Snapshot[M] = Snapshot(Index(0),List.empty,Set.empty[IndicationFrom[M#Ind]],step,None)

  final def toRequest[R](reqPayload: R => String)(p: ProcessId, req: ProcessInput[R], i: Index): Action = req match {
    case ProcessRequest(_, req) => Request(p, i, reqPayload(req))
    case Crash(_) => Crashed(p, i)
  }

  final def toIndication[Ind](indicationPayload: Ind => String)(i: Index)(ind: IndicationFrom[Ind]) = Indication(ind.p, i, indicationPayload(ind.i))

  def sendToUndelivered[P](index: Index)(send: FLLSend[P]): Undelivered = Undelivered(send.packet.from, send.packet.to, send.packet.id, send.packet.payload.toString, index)

  def next[M <: ModT](indicationPayload: M#Ind => String, reqPayload: M#Req => String)(snapshot: Snapshot[M], op: Op[M]): Snapshot[M] = (snapshot, op) match {
    case (c@Snapshot(current, actions, pastInd, wr@WaitingRequest(_), _), NextReq(req)) =>
      val RequestResult(sent, ind, wd) = wr.request(req.requests.values.toSeq)
      val sends = sent.map(sendToUndelivered(current))
      val requests = req.requests.map { case (p, r) => toRequest(reqPayload)(p, r, current) }
      val indications = ind.map(toIndication(indicationPayload)(current))
      Snapshot(current.next, actions ++ indications ++ requests ++ sends, pastInd ++ ind, wd, Some(c))
    case (c@Snapshot(current, actions,pastInd, wd@WaitingDeliver(_), _), NextDeliver(del)) =>
      val DeliverResult(sent, ind, wr) = wd.deliver(del)
      val sends = sent.map(sendToUndelivered(current))
      val delivers = del.ops.values.flatMap {
        case Left(FLLDeliver(Packet(id, payload, from, to, _))) => Some(Delivered(from, to, id, payload.toString, actions.collectFirst {
          case Undelivered(`from`, `to`, `id`, _, s) => s
        }.get, current)) // FIXME refactor code
        case _ => None
      }
      val drops = del.ops.values.flatMap {
        case Right(Drop(Packet(id, payload, from, to, instance))) => Some(Dropped(from, to, id, payload.toString, actions.collectFirst {
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
      Snapshot(current.next, filtered ++ indications ++ delivers ++ sends ++ drops, pastInd ++ ind, wr, Some(c))
    case (Snapshot(_, _, _,_, Some(prev)), Prev()) => prev
    case (Snapshot(_, _, _,_, Some(prev)), Reset()) => next(indicationPayload, reqPayload)(prev, Reset()) // FIXME TEMPORAL SOLUTION, store a list of all snapshots instead. or some structure that allows access to root in O (1)
    case (s, input) =>
      //org.scalajs.dom.console.log(s"%c Bad input $input for step ${s.step} ", "background: #222; color: #bada55")
      s
  }

  type Conditions[M <: oss.giussi.cappio.Mod] = List[Condition[Snapshot[M]]]

}

case class Snapshot[M <: ModT](index: Index, actions: List[Action], indications: Set[IndicationFrom[M#Ind]], step: Step[M], prev: Option[Snapshot[M]]) {
  def last = LastSnapshot(index, actions)
}

case class LevelId(x: Int) {
  lazy val next = LevelId(x + 1)
}

sealed trait LevelResult

case object LevelPassed extends LevelResult

case object Pending extends LevelResult

object AbstractLevel {

  // TODO
  def apply[M <: ModT](scheduler: Scheduler[M], conditions: Conditions[M])(implicit show: Show[M#Payload], show2: Show[M#Req]): Level = new AbstractLevel[M](scheduler, conditions)(show, show2){
    override val indicationPayload: M#Ind => String = ???
    override val reqTypes: List[Inputs[Req]] = ???
  }
}

abstract class AbstractLevel[M <: ModT](scheduler: Scheduler[M], conditions: Conditions[M] = List.empty)(implicit show: Show[M#Payload], show2: Show[M#Req]) extends Level {

  type Payload = M#Payload
  type State = M#State
  type Ind = M#Ind
  type Req = M#Req
  type ReqBatch = RequestBatch[Req]
  type DelBatch = DeliverBatch[M#Payload]

  val processes = Processes(scheduler.processes.keySet)

  case class ProcessState(id: ProcessId, state: State, status: ProcessStatus)

  val $next = new EventBus[Op[M]]

  final def requestPayload(req: M#Req): String = show2.show(req)

  // TODO use typeclass show?
  val indicationPayload: M#Ind => String

  val $snapshots: Signal[Snapshot[M]] = {
    val ns = Snapshot.next[M](indicationPayload, requestPayload) _
    $next.events.fold(Snapshot.init(WaitingRequest(scheduler)))(ns)
  }

  val $steps: Signal[Step[M]] = $snapshots.map(_.step)

  override val $actions = $snapshots.map(_.last)

  val reqTypes: List[Inputs[Req]]

  override def actionSelection: Div = {
    div(
      div(
        child <-- $steps.map {
          case WaitingRequest(sch) => ActionSelection.reqBatchInput(reqTypes, sch.availableProcesses.toList, $next.writer.contramap[RequestBatch[M#Req]](r => NextReq(r)))
          case WaitingDeliver(sch) => ActionSelection.networkInput(sch.availablePackets, $next.writer.contramap[DelBatch](r => NextDeliver(r)))
        }
      ),
      div(
        button(
          cls := "btn btn-secondary",
          onClick.mapToValue(Prev[M]()) --> $next.writer,
          "Previous"
        ),
        button(
          cls := "btn btn-secondary",
          onClick.mapToValue(Reset[M]()) --> $next.writer,
          "Reset"
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
    // https://stackoverflow.com/a/10635041
    //children <-- $states.split(_.id)(renderState) TODO
  )

  override val $conditions = {
    val c = conditions.zipWithIndex.map { case (condition, index) => condition.andThen(ConditionLevel(index, _: ConditionResult)) }
    $snapshots.map(snap => c.map(_.apply(snap)))
  }

  override def status: EventStream[LevelPassed.type] = $conditions.changes.filter(_.find(!_.ok).isEmpty).map(_ => LevelPassed)


}