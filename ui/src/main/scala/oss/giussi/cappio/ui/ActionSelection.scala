package oss.giussi.cappio.ui

import java.util.UUID

import com.raquo.laminar.api.L._
import com.raquo.laminar.nodes.ReactiveHtmlElement
import org.scalajs.dom
import org.scalajs.dom.html
import oss.giussi.cappio.Network.InTransitPacket
import oss.giussi.cappio.{DeliverBatch, Drop, FLLDeliver, Packet, ProcessId, Processes, RequestBatch}
import oss.giussi.cappio.impl.bcast.BestEffortBroadcast.BebBcast
import oss.giussi.cappio.impl.bcast.UniformReliableBroadcast.Payload

object ActionSelection {

  type Select = ReactiveHtmlElement[dom.html.Select]


  /*
  def baseSelect(options: List[String]) = {
    select(
      cls := "browser-default custom-select mb-4"
    )
  }

   */

  def networkInput(available: Set[InTransitPacket], $out: Observer[DeliverBatch]) = {
    val $batch = Var(DeliverBatch.empty)

    def renderPacket(over: Var[Option[UUID]])(id: UUID, initial: PacketSelection, $changes: Signal[PacketSelection]) = {
      def renderAvailable(inTransit: InTransitPacket) = {
        div(
          hidden <-- over.signal.map(!_.contains(id)),
          button(`type` := "button", cls := "btn btn-primary btn-sm", "Drop",
            onClick.mapTo(inTransit.drop) --> Observer.apply[Drop](d => $batch.update(_.add(d)))
          ),
          button(`type` := "button", cls := "btn btn-primary btn-sm", "Deliver",
            onClick.mapTo(inTransit.deliver) --> Observer.apply[FLLDeliver](d => $batch.update(_.add(d)))
          )
        )
      }

      def renderSelection(update: DeliverBatch => DeliverBatch, text: String) = {
        div(
          button(cls := "btn btn-danger", text,
            span(
              cls := "badge badge-danger ml-3", "X",
              onClick.mapToValue(()) --> Observer.apply[Unit](_ => $batch.update(update)) // TODO esta bien?
            )
          )
        )
      }

      def renderDrop(drop: Drop) = renderSelection(_.remove(drop),"Drop")

      def renderDeliver(deliver: FLLDeliver) = renderSelection(_.remove(deliver),"Deliver")

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
    sealed trait PacketSelection {
      def packet: Packet
    }
    case class AvailablePacket(p: InTransitPacket) extends PacketSelection {
      override def packet = p.packet
    }
    case class SelectedDrop(d: Drop) extends PacketSelection {
      override def packet = d.packet
    }
    case class SelectedDeliver(d: FLLDeliver) extends PacketSelection {
      override def packet = d.packet
    }
    val over: Var[Option[UUID]] = Var(None)
    val $available = available.map(AvailablePacket)
    val $selected = $batch.signal.map(_.ops.values.map {
      case Right(d) => SelectedDrop(d)
      case Left(d) => SelectedDeliver(d)
    }.toSet)
    // FIXME ObserverError: TypeError: prevChildRef.elem$1 is null
    val $all = $selected.map(s => ($available.filterNot(s.contains) ++ s).toList.sortBy(_.packet.id))

    div(
      ul(cls := "list-group list-group-flush",
        children <-- $all.split(_.packet.id)(renderPacket(over)) // TODO siempre haciendo toList!
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
  def reqInput[Req, Pay](processes: Processes, reqWriter: Observer[AddReq[Req]], inputPayload: Observer[Option[Req]] => String => Input) = {
    val process: Var[Option[ProcessId]] = Var(None) // TODO why use Var if I don't make use of set?
    val request: Var[Option[String]] = Var(None)
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
        div(cls := "col-md-4 mb-3",
          label(forId := "processId", "Process"),
          selectProcess(processes.all, process.writer, id := "processId"),
        ),
        div(cls := "col-md-4 mb-3",
          label(forId := "reqType", "Request"),
          selectReqType(List("beb"), request.writer, id := "requestType")
        ),
        div(cls := "col-md-4 mb-3",
          label(forId := "payload", "Payload"), // TODO pass id
          child <-- request.signal.changes.map {
            case Some(s) => iPayload(s)
            case None => label("")
          }
        ),
        div(cls := "col-md-4 mb-3",
          button(`type` := "button", cls := "btn btn-success btn-sm", i(cls := "fas fa-plus"),
            disabled <-- req.map(_.isEmpty),
            onClick.mapToValue(()) --> $add.writer
          )
        )
      ),
    )
    req.changes
      .filter(_.isDefined)
      .map(_.get) // TODO flatMap?
      .combineWith($add.events) // FIXME esto no funciona porque si yo emito una vez un click dsp combina todo con eso. Yo quiero un zip!
      .map(_._1)
      .addObserver(reqWriter)(f)
    f
  }

  sealed trait BatchCommand

  case object Reset extends BatchCommand

  case class AddReq[Req](id: ProcessId, r: Req) extends BatchCommand

  case class RemoveReq(id: ProcessId) extends BatchCommand


  def reqBatchInput[Req](processes: Processes, $obs: Observer[RequestBatch[Req]], inputPayload: Observer[Option[Req]] => String => Input) = {
    def renderReq[Req]($rem: Observer[RemoveReq])(to: ProcessId, initial: (ProcessId, Req), $changes: Signal[(ProcessId, Req)]): Div = div(
      initial.toString(),
      button(`type` := "button", cls := "btn btn-danger btn-sm", i(cls := "fas fa-minus"),
        onClick.mapToValue(RemoveReq(to)) --> $rem
      )
    )

    val $commands = new EventBus[BatchCommand]
    val $batch: Signal[RequestBatch[Req]] =
      $commands.events.fold(RequestBatch[Req](Map.empty)) {
        case (_, Reset) => RequestBatch[Req](Map.empty)
        case (batch, AddReq(id, r: Req)) => batch.add(id, r)
        case (batch, RemoveReq(id)) => batch.remove(id)
      }
    div(
      children <-- $batch.map(_.requests.toList).split(_._1)(renderReq($commands.writer)),
      reqInput(processes, $commands.writer, inputPayload),
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
  def selectReqType(types: List[String], obs: Observer[Option[String]], modifiers: Modifier[Select]*) = {
    //val request: Var[Option[String]] = Var(None)
    select(
      cls := "browser-default custom-select mb-4",
      inContext(thisNode => onInput.mapTo(thisNode.ref.value).map(Option(_).filterNot(_ == "unset")) --> obs),
      option(
        "-",
        value := "unset"
      ) :: types.map(p => option(
        p.toString,
        value := p.toString,
      )),
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

  def bebBroadcast(processes: List[ProcessId], beb: WriteBus[(ProcessId, BebBcast)]) = {
    val process = new EventBus[Option[ProcessId]]
    val payload = new EventBus[Option[String]]
    val comb = process.events.combineWith(payload.events)
      .map { case (o1, o2) => for {
        pid <- o1
        pay <- o2
      } yield (pid, BebBcast(Payload(pay), null))
      }.filter(_.isDefined).map(_.get)

    val elem = div(
      "BebBcast",
      selectProcess(processes, process.writer),
      input(
        inContext(thisNode => onChange.mapTo(thisNode.ref.value).map(v => Option(v).filterNot(_.isEmpty)) --> payload)
      )
    )
    beb.addSource(comb)(elem)
    elem
  }


}
