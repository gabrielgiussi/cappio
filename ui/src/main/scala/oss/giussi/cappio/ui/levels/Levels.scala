package oss.giussi.cappio.ui.levels

import com.raquo.laminar.api.L._
import com.raquo.laminar.nodes.ReactiveHtmlElement
import org.scalajs.dom.raw.HTMLElement
import oss.giussi.cappio
import oss.giussi.cappio.Conditions.ConditionWithDescription
import oss.giussi.cappio.impl.net.FairLossLink.FLLSend
import oss.giussi.cappio.ui.ActionSelection.Inputs
import oss.giussi.cappio.ui.core._
import oss.giussi.cappio.ui.levels.Snapshot.Conditions
import oss.giussi.cappio.ui.levels.bcast.{BEBLevel, CRDTLevel, CausalLevel, RBLevel, URBLevel}
import oss.giussi.cappio.ui._
import oss.giussi.cappio.{Mod => ModT, _}

object Levels {

  val RAW_LEVELS: List[LevelId => Selection] = List(
    Documentation(Introduction.source) _,
    _ => DemoLevel.good(4, 3),
    _ => BEBLevel.simple(4, 3),
    _ => BEBLevel.broken(4, 3),
    _ => RBLevel(4, 3),
    _ => CausalLevel.good(4, 3),
    _ => CRDTLevel.good(4, 3)
  )

  val INDEXED_LEVELS: Map[LevelId, IndexedLevel] = RAW_LEVELS.zipWithIndex.map { case (level, index) =>
    val levelId = LevelId(index)
    levelId -> IndexedLevel(index, level(levelId))
  }.toMap

  val LEVELS = INDEXED_LEVELS.values.toList.sortBy(_.x)

  val $pendingLevels: StrictSignal[Map[LevelId, LevelResult]] = {
    val pendingLevels: Var[Map[LevelId, LevelResult]] = Var(INDEXED_LEVELS.map { case (k, _) => k -> Pending })

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

case class Documentation(doc: ReactiveHtmlElement[org.scalajs.dom.html.Div])(id: LevelId) extends Selection {
  override def render = div(cls := "container-fluid mt-5",
    div(cls := "row wow fadeIn",
      div(cls := "col-md mb-4",
        div(cls := "card",
          div(cls := "card-body",
            doc
          ),
          a(href := s"#${id.next.x}",
            cls := "btn btn-primary",
            role := "button",
            "Siguiente"
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

trait Level[M <: oss.giussi.cappio.Mod] extends Selection {

  val state = new AppStore[M]

  val processes: Processes

  val $actions: Signal[LastSnapshot]

  def diagram: Div = Diagram(processes, $actions)

  def actionSelection: Div

  def states: Div

  val $conditions: Signal[List[ConditionLevel]]

  val description: Div

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

  def prevAndReset = div(cls := "d-flex justify-content-end",
    button(
      cls := "btn btn-link p-0",
      span(
        cls := "fas fa-reply",
        onClick.mapToValue(Prev[M]()) --> state.ops.writer,
      )
    ),
    button(
      cls := "btn btn-link p-0",
      span(
        cls := "fas fa-redo",
        onClick.mapToValue(Reset[M]()) --> state.ops.writer
      )
    )
  )


  def diagramPanel = div(
    div(cls := "row wow",
      div(cls := "col-md-9 mb-4",
        div(cls := "card",
          div(cls := "card-body",
            prevAndReset,
            diagram
          )
        )
      ),
      div(cls := "col-md-3 mb-4",
        div(cls := "card mb-4",
          div(cls := "card-header text-center", "Selección de Acciones"),
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

  def goalsPanel = div(cls := "row wow",
    div(cls := "col mb-4",
      div(cls := "card",
        div(cls := "card-body",
          description
        )
      ),
    )
  )

  final override def render = {
    def tablink(title: String, target: String, active: Boolean = false) = li(cls := "nav-item",
      a(cls := s"nav-link ${if (active) "active" else ""}",
        id := s"tab-to-$target",
        dataAttr("toggle") := "tab",
        href := s"#$target",
        role := "tab",
        title
      )
    )

    def tabcontent(target: String, content: Div, active: Boolean = false) = div(
      cls := s"tab-pane fade show ${if (active) "active" else ""}",
      id := target,
      role := "tabpanel",
      content
    )

    div(cls := "container-fluid mt-5",
      ul(cls := "nav nav-tabs",
        id := "myTab",
        role := "tablist",
        tablink("Descripcion", "goals-panel", true),
        tablink("Diagrama", "diagram-panel"),
      ),
      div(cls := "tab-content",
        id := "myTabContent",
        tabcontent("goals-panel", goalsPanel, true),
        tabcontent("diagram-panel", diagramPanel)
      )
    )
  }

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

  def init[M <: oss.giussi.cappio.Mod](step: Step[M], predefined: Set[PredefinedAction[M#Req]], describeReq: M#Req => ActionDescription): Snapshot[M] = {
    val asActions = predefined.collect {
      case PredefinedAction(index,id,request) => toRequest(describeReq)(id,request,index,true)
        // case Crashed
    }
    Snapshot(Index(0), asActions.toList, Set.empty[IndicationFrom[M#Ind]], step, None)
  }

  // FIXME estoy agarrando predefined como parametro en lugar de tomarlo de input, porque si lo tomo de input siempre es true!
  final def toRequest[R](reqPayload: R => ActionDescription)(p: ProcessId, input: ProcessInput[R], i: Index, predefined: Boolean): Action = input match {
    case ProcessRequest(_, req,_) =>
      val ActionDescription(name, payload,tags) = reqPayload(req)
      Actions.request(name.getOrElse(""), p, i, payload,predefined,tags) // FIXME getOrElse
    case Crash(_,_) => Crashed(p, i)
  }

  final def toIndication[Ind](indicationPayload: Ind => ActionDescription)(i: Index)(ind: IndicationFrom[Ind]) = {
    val ActionDescription(name,payload,tags) = indicationPayload(ind.i)
    Actions.indication(name.getOrElse(""), ind.p, i, payload, tags)
  }

  def sendToUndelivered[P](f: P => ActionDescription)(index: Index)(send: FLLSend[P], alreadyDelivered: Boolean): Option[Undelivered] = send match {
    case FLLSend(Packet(id, payload, from, to, _)) =>
      if (from == to) None
      else {
        val ActionDescription(_,pay,tags) = f(payload)
        Some(Actions.undelivered(from, to, id, pay, index, alreadyDelivered, tags))
      }
  }

  def sendToDelivered[P](f: P => ActionDescription)(current: Index)(packet: Packet[P], sent: Index) = {
    val Packet(id,p,from,to,_) = packet
    val ActionDescription(_,payload,tags) = f(p)
    Actions.delivered(from,to,id,payload,sent,current,tags)
  }

  def wasDelivered[P](network: Network[P], s: FLLSend[P]) = network.alreadyDelivered.contains(s.packet)

  // TODO refactor required
  def next[M <: ModT](implicit describe: DescribeModuleAction[M]): (Snapshot[M], Op[M]) => Snapshot[M] = {
    val toInd = toIndication(describe.describeInd) _
    val toReq = toRequest(describe.describeReq) _
    val toDel = sendToDelivered(describe.describePayload) _

    (snapshot: Snapshot[M], op: Op[M]) =>
      (snapshot, op) match {
        case (c@Snapshot(current, actions, pastInd, wr@WaitingRequest(Scheduler(_, network, _, _)), _), NextReq(req)) =>
          val RequestResult(sent, ind, wd) = wr.request(req.requests.values.toSeq)
          val sends = sent.flatMap(s => sendToUndelivered(describe.describePayload)(current)(s, wasDelivered(network, s)))
          val requests = req.requests.map { case (p, r) => toReq(p, r, current, false) }
          val indications = ind.map(toInd(current))
          val newActions = (actions ++ requests).groupBy(_.id).values.map { // FIXME quick fix to eliminate duplicates caused by predefined actions
            case a :: Nil => a
            case (r: Request) :: (_: Request) :: Nil =>
              println(r)
              r.copy(predefined = false)
          }.toList ++ indications ++ sends
          Snapshot(current, newActions, pastInd ++ ind, wd, Some(c))
        case (c@Snapshot(current, actions, pastInd, wd@WaitingDeliver(Scheduler(_, network, _, _)), _), NextDeliver(del)) =>
          val result = if (del.ops.isEmpty) wd.tick else wd.deliver(del)
          val sent = result.sent
          val ind = result.ind
          val (nextIndex, wd2) = result match {
            case DeliverResult(_, _, w) => (current.next, w)
            case RequestResult(_, _, w) => (current, w)
          }
          val sends = sent.flatMap(s => sendToUndelivered(describe.describePayload)(current)(s, wasDelivered(network, s)))
          val delivers = del.ops.values.flatMap {
            case Left(FLLDeliver(packet@Packet(id, _, from, to, _))) => Some(toDel(current)(packet, actions.collectFirst {
              case Undelivered(`from`, `to`, `id`, _, s, _,_) => s
            }.get)) // FIXME refactor code
            case _ => None
          }
          val drops = del.ops.values.flatMap {
            case Right(Drop(Packet(id, payload, from, to, _))) => Some(Dropped(from, to, id, payload.toString, actions.collectFirst {
              case Undelivered(`from`, `to`, `id`, _, s, _,_) => s
            }.get, current))
            case _ => None
          }
          val indications = ind.map(toInd(current))
          val filtered = {
            val deliversKeys = delivers.map(_.id).toSet
            val dropsKeys = drops.map(_.id).toSet
            actions.filter {
              case u: Undelivered if deliversKeys.contains(u.id) => false
              case u: Undelivered if dropsKeys.contains(u.id) => false
              case _ => true
            }
          }
          Snapshot(nextIndex, filtered ++ indications ++ delivers ++ sends ++ drops, pastInd ++ ind, wd2, Some(c))
        case (Snapshot(_, _, _, _, Some(prev)), Prev()) => prev
        case (Snapshot(_, _, _, _, Some(prev)), Reset()) => next(describe)(prev, Reset()) // FIXME TEMPORAL SOLUTION, store a list of all snapshots instead. or some structure that allows access to root in O (1)
        case (s, input) =>
          //org.scalajs.dom.console.log(s"%c Bad input $input for step ${s.step} ", "background: #222; color: #bada55")
          s
      }
  }

  type Conditions[M <: oss.giussi.cappio.Mod] = List[ConditionWithDescription[Snapshot[M]]]

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

  /* TODO
  def apply[M <: ModT](scheduler: Scheduler[M], conditions: Conditions[M])(implicit show: Show[M#Payload], show2: Show[M#Req]): Level = new AbstractLevel[M](scheduler, conditions)(show, show2){
    override val indicationPayload: M#Ind => String = ???
    override val reqTypes: List[Inputs[Req]] = ???
    override val shortDescription: Div = ???
  }
   */
}

case class PredefinedAction[R](index: Index, processId: ProcessId, action: ProcessInput[R])

abstract class AbstractLevel[M <: ModT](scheduler: Scheduler[M], conditions: Conditions[M] = List.empty, actions: List[PredefinedAction[M#Req]] = List.empty)(implicit show: Show[M#Payload],
                                                                                                         show2: Show[M#Req],
                                                                                                         showDOM: ShowDOM[M#State],
                                                                                                         describe: DescribeModuleAction[M]) extends Level[M] {

  type Payload = M#Payload
  type State = M#State
  type Ind = M#Ind
  type Req = M#Req
  type ReqBatch = RequestBatch[Req]
  type DelBatch = DeliverBatch[M#Payload]

  val allConditions = {
    if (predefined.nonEmpty) {
      val conditionPredefined = LevelConditions.predefinedActions[M](predefined, r => {
        Snapshot.toRequest(describe.describeReq)(r.processId, r.action, r.index, true) // TODO ugly as shit!
      })
      conditions :+ conditionPredefined
    }
    else conditions
  }

  val processes = Processes(scheduler.processes.keySet)

  def predefined: Set[PredefinedAction[M#Req]] = Set.empty

  case class ProcessState(id: ProcessId, state: State, status: ProcessStatus)

  def requestPayload(req: M#Req): (String, String) = ("", "")

  // TODO use typeclass show?
  val indicationPayload: M#Ind => String

  val $snapshots: Signal[Snapshot[M]] = {
    val ns = Snapshot.next[M]
    state.ops.events.fold(Snapshot.init(WaitingRequest(scheduler),predefined, describe.describeReq))(ns)
  }

  val $steps: Signal[(Step[M],Index)] = $snapshots.map(s => (s.step,s.index))

  override val $actions = $snapshots.map(_.last)

  val $states: Signal[List[ProcessState]] = $steps.map(_._1.scheduler.processes.values.toList.sortBy(_.id.id).map { case Process(id, stack, status) => ProcessState(id, stack.state, status) })

  val reqTypes: List[Inputs[Req]]

  override def actionSelection: Div = {
    div(
      div(
        child <-- $steps.map {
          case (WaitingRequest(sch),index) =>
            val forThisStep = predefined
              .filter(a => sch.processes.get(a.processId).map(_.status).contains(Up))
              .filter(_.index == index).map { case PredefinedAction(_,id,action) => (id,action) }.toMap
            ActionSelection.reqPredefined(forThisStep)(List[Inputs[Req]](ActionSelection.crash), sch.availableProcesses.toList, state.ops.writer.contramap[RequestBatch[M#Req]](r => NextReq(r)))
          case (WaitingDeliver(sch),_) =>
            ActionSelection.networkInput(sch.availablePackets(false), state.ops.writer.contramap[DelBatch](r => NextDeliver(r)))
        }
      )
    )
  }

  import oss.giussi.cappio.ui.ShowDOMSyntax._

  def renderState(processId: ProcessId, initial: ProcessState, $states: Signal[ProcessState]) = div(cls := "col-md-4 mb-4",
    div(cls := "card",
      div(
        cls := "card-header",
        span(cls := "badge badge-pill",
          s"Proceso ${processId.id}",
          cls <-- $states.map { case ProcessState(_, _, Up) => "badge-success" case _ => "badge-danger" },
        )
      ),
      div(
        id := s"processState${processId.toString}",
        cls := "card-body px-3 py-1",
        child <-- $states.map(_.state.toDOM) // FIXME I should pass the Stream to the function toDom for better performance! (now is regenerating all divs each time)
      )))

  override def states: Div = div(
    cls := "row wow fadeIn",
    children <-- $states.split(_.id)(renderState)
  )

  override val $conditions = {
    val c = allConditions.zipWithIndex.map { case (condition, index) => condition.andThen(ConditionLevel(index, _: ConditionResult)) }
    $snapshots.map(snap => c.map(_.apply(snap)))
  }

  override def status: EventStream[LevelPassed.type] = $conditions.changes.filter(_.find(!_.ok).isEmpty).map(_ => LevelPassed)

  val shortDescription: Div

  lazy final val description = div(
    shortDescription,
    p("Para pasar este nivel vas a tener que cumplir con los siguientes objetivos"),
    ul(cls := "list-group list-group-flush",
      allConditions.map { case ConditionWithDescription(short, full, _) =>
        li(cls := "list-group-item",
          h3(short),
          p(full)
        )
      }
    )
  )
}