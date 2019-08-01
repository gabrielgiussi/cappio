package oss.giussi.cappio.ui

import com.raquo.laminar.api.L._
import com.raquo.laminar.nodes.ReactiveHtmlElement
import org.scalajs.dom
import oss.giussi.cappio.Network.InTransitPacket
import oss.giussi.cappio._
import oss.giussi.cappio.ui.levels.RequestBatch

object ActionSelection {

  type Select = ReactiveHtmlElement[dom.html.Select]


  /*
  def baseSelect(options: List[String]) = {
    select(
      cls := "browser-default custom-select mb-4"
    )
  }

   */

  def payloadInput(obs: Observer[Option[String]]) = div(
    //cls := "md-form",
    input(
      `type` := "text",
      cls := "form-control",
      id := "payload-input",
      // TODO how to throttle key press?
      inContext(thisNode => onChange.mapTo(Option(thisNode.ref.value).filterNot(_.isEmpty)) --> obs),
    )
  )

  def networkInput[P](available: Set[InTransitPacket[P]], $out: Observer[DeliverBatch[P]]) = {
    val $batch = Var(DeliverBatch.empty[P])

    def renderPacket(over: Var[Option[Packet[P]]])(id: Packet[P], initial: PacketSelection[P], $changes: Signal[PacketSelection[P]]) = {
      def renderAvailable(inTransit: InTransitPacket[P]) = {
        div(
          hidden <-- over.signal.map(!_.contains(id)),
          button(`type` := "button", cls := "btn btn-primary btn-sm", "Drop",
            onClick.mapTo(inTransit.drop) --> Observer.apply[Drop[P]](d => $batch.update(_.add(d)))
          ),
          button(`type` := "button", cls := "btn btn-primary btn-sm", "Deliver",
            onClick.mapTo(inTransit.deliver) --> Observer.apply[FLLDeliver[P]](d => $batch.update(_.add(d)))
          )
        )
      }

      def renderSelection(update: DeliverBatch[P] => DeliverBatch[P], text: String) = {
        div(
          button(cls := "btn btn-danger", text,
            span(
              cls := "badge badge-danger ml-3", "X",
              onClick.mapToValue(()) --> Observer.apply[Unit](_ => $batch.update(update)) // TODO esta bien?
            )
          )
        )
      }

      def renderDrop(drop: Drop[P]) = renderSelection(_.remove(drop), "Drop")

      def renderDeliver(deliver: FLLDeliver[P]) = renderSelection(_.remove(deliver), "Deliver")

      li(cls := "list-group-item",
        s"From ${initial.packet.from} to ${initial.packet.to} ${initial.packet.payload}", // TODO
        onMouseEnter.mapToValue(Some(id)) --> over.writer,
        onMouseLeave.mapToValue(None) --> over.writer,
        child <-- $changes.map {
          case AvailablePacket(p) => renderAvailable(p)
          case SelectedDrop(d) => renderDrop(d)
          case SelectedDeliver(d) => renderDeliver(d)
        }
      )
    }

    // son necesarias las 3 clases o con un enum alcanza?
    sealed trait PacketSelection[P] {
      def packet: Packet[P]
    }
    case class AvailablePacket[P](p: InTransitPacket[P]) extends PacketSelection[P] {
      override def packet = p.packet
    }
    case class SelectedDrop(d: Drop[P]) extends PacketSelection[P] {
      override def packet = d.packet
    }
    case class SelectedDeliver(d: FLLDeliver[P]) extends PacketSelection[P] {
      override def packet = d.packet
    }
    val over: Var[Option[Packet[P]]] = Var(None)
    val $available = available.map(AvailablePacket.apply)
    val $selected = $batch.signal.map(_.ops.values.map {
      case Right(d) => SelectedDrop(d)
      case Left(d) => SelectedDeliver(d)
    }.toSet)
    // FIXME ObserverError: TypeError: prevChildRef.elem$1 is null
    val $all = $selected.map(s => ($available.filterNot(s.contains) ++ s).toList.sortBy(_.packet.id))

    div(
      ul(cls := "list-group list-group-flush",
        children <-- $all.split(_.packet)(renderPacket(over)) // TODO siempre haciendo toList!
      ),
      button(`type` := "reset", cls := "btn btn-primary", "Next",
        onClick.mapTo($batch.now()) --> $out
      ),
      button(`type` := "button", cls := "btn btn-danger", "Clear",
        disabled <-- $batch.signal.map(_.ops.isEmpty),
        onClick.mapToValue(()) --> Observer.apply[Unit](_ => $batch.set(DeliverBatch.empty))
      )
    )
  }

  // TODO como limpiar el form cuando submiteo un req!?
  /*
  no me gusta el tipo de inputPayload. 2 cosas
  (1) deberia ser un EventStream o un ReactiveElement?
  (2) no hay una mejor manera de tomar los cambios sin la necesidad de parle el Observer? Tal vez hacer un elemento
  que sea un ReactiveElement y que me de un events tipado del estilo
  val sel = selectRequest()
  sel.events(onSelection) // pero q el onSelecion lo defina yo, no que sean dom events

   */
  def reqInput[Req, Pay, ReqType](reqTypes: List[ReqType], processes: List[ProcessId], reqWriter: Observer[AddReq[Req]], inputPayload: Observer[Option[Req]] => ProcessId => ReqType => ReactiveHtmlElement[dom.html.Element]) = {
    val process: Var[Option[ProcessId]] = Var(None) // TODO why use Var if I don't make use of set?
    val requestType: Var[Option[ReqType]] = Var(None)
    val payload: Var[Option[Req]] = Var(None)
    val iPayload = inputPayload(payload.writer)
    val req = process.signal
      .combineWith(payload.signal)
      .map { case (process, payload) => for {
        id <- process
        p <- payload
      } yield AddReq(id, p)
      }
    val $add = new EventBus[Unit]
    val f = form(
      div(cls := "form-row",
        div(cls := "col-sm mb-3",
          label(forId := "processId", "Process"),
          selectProcess(processes, process.writer, id := "processId")
        ),
        div(cls := "col-sm mb-3",
          label(forId := "reqType", "Request"),
          selectReqType(reqTypes, requestType.writer, id := "requestType")
        ),
        div(cls := "col-sm mb-3",
          label(forId := "payload", "Payload"), // TODO pass id
          child <-- requestType.signal.changes.map { rtype =>
            (for {
              r <- rtype
              pid <- process.now()
            } yield iPayload(pid)(r)) getOrElse label("")
          }
        ),
        div(cls := "col-sm mb-3",
          button(`type` := "button", cls := "close",
            span(
              cls := "fas fa-plus",
              cls <-- req.map(r => if (r.isDefined) "green-text" else "")
            ),
            disabled <-- req.map(_.isEmpty),
            onClick.mapToValue(()) --> $add.writer
          )
        )
      )
    )
    req.changes
      .filter(_.isDefined)
      .map(_.get) // TODO flatMap?
      .combineWith($add.events) // FIXME esto no funciona porque si yo emito una vez un click dsp combina todo con eso. Yo quiero un zip!
      .map { case (add,_) =>
        process.set(None)
        payload.set(None)
        requestType.set(None)
        add
      }
      .addObserver(reqWriter)(f)
    f
  }

  sealed trait BatchCommand

  case object Reset extends BatchCommand

  case class AddReq[Req](id: ProcessId, r: Req) extends BatchCommand

  case class CrashProcess(id: ProcessId) extends BatchCommand

  case class RemoveReq(id: ProcessId) extends BatchCommand


  def reqBatchInput[Req, ReqType](reqTypes: List[ReqType], processes: List[ProcessId], $obs: Observer[RequestBatch[Req]], inputPayload: Observer[Option[Req]] => ProcessId => ReqType => ReactiveHtmlElement[dom.html.Element]) = {
    val $commands = new EventBus[BatchCommand]
    val $batch: Signal[RequestBatch[Req]] =
      $commands.events.fold(RequestBatch[Req](Map.empty)) {
        case (_, Reset) => RequestBatch[Req](Map.empty)
        case (batch, AddReq(id, r: Req)) => batch.add(id, r) // FIXME unchecked
        case (batch, RemoveReq(id)) => batch.remove(id)
        case (batch,CrashProcess(id)) => batch.crash(id)
      }

    def renderBatch = {
      def renderReq[R](to: ProcessId, initial: (ProcessId, ProcessInput[R]), $changes: Signal[(ProcessId, ProcessInput[R])]) = tr(
        th(
          //scope := "",
          to.toString
        ),
        td(
          child <-- $changes.map {
            case (_,ProcessRequest(_,r)) => label(r.toString)
            case (_,Crash(_)) => label("Crash")
          }
        ),
        td(
          button(`type` := "button", cls := "close",
            span(className := "fas fa-times red-text"),
            onClick.mapToValue(RemoveReq(to)) --> $commands
          )
        )
      )
      table(
        cls := "table",
        tbody(
          children <-- $batch.map(_.requests.toList).split(_._1)(renderReq)
        )
      )
    }

    def crashInput(processes: List[ProcessId], reqWriter: Observer[CrashProcess]) = {
      val process: Var[Option[ProcessId]] = Var(None)
      form(
        div(cls := "form-row",
          div(cls := "col-sm mb-3",
            label(forId := "processId", "Process"),
            selectProcess(processes, process.writer, id := "processId")
          ),
          div(cls := "col-sm mb-3",
            button(`type` := "button", cls := "close",
              span(
                cls := "fas fa-plus",
                cls <-- process.signal.map(r => if (r.isDefined) "green-text" else "")
              ),
              disabled <-- process.signal.map(_.isEmpty),
              onClick.mapTo(process.now().map(CrashProcess)).filter(_.isDefined).map(_.get) --> reqWriter
            )
          )
        )
      )
    }

    div(
      reqInput(reqTypes, processes, $commands.writer, inputPayload),
      crashInput(processes,$commands.writer),
      renderBatch,
      input(`type` := "submit", cls := "btn btn-primary", value := "Next", // TODO input or button?
        inContext { thisNode =>
          val a = $batch.observe(thisNode)
          onClick.preventDefault.mapTo(a.now()) --> $obs
        }
      ),
      input(`type` := "reset", cls := "btn btn-danger", value := "Clear",
        disabled <-- $batch.map(_.requests.isEmpty),
        onClick.preventDefault.mapToValue(Reset) --> $commands
      )
    )
  }

  // TODO esto depende de cada nivel
  // auto trigger and disable if there is only one option
  def selectReqType[ReqType](types: List[ReqType], obs: Observer[Option[ReqType]], modifiers: Modifier[Select]*) = {
    //val request: Var[Option[String]] = Var(None)
    val indexed = types.zipWithIndex.map { case (rt, index) => index -> rt }.toMap
    select(
      cls := "browser-default custom-select mb-4",
      inContext(thisNode => onInput.mapTo(thisNode.ref.value).map(v => indexed.get(v.toInt)) --> obs),
      option(
        "-",
        value := "-1"
      ) :: indexed.map { case (i, rt) => option(
        rt.toString,
        value := i.toString
      )
      }.toList,
      modifiers
    )
  }

  def selectProcess(processes: List[ProcessId], obs: Observer[Option[ProcessId]], modifiers: Modifier[Select]*) = {
    select(
      cls := "browser-default custom-select mb-4",
      inContext(thisNode => onInput.mapTo(thisNode.ref.value).map(Option(_).filterNot(_ == "unset").map(_.toInt).map(ProcessId)) --> obs),
      option(
        "-",
        value := "unset"
      ) :: processes.map(p => option(
        p.id.toString,
        value := p.id.toString,
      )),
      modifiers
    )
  }

}
