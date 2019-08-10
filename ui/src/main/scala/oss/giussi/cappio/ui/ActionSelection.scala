package oss.giussi.cappio.ui

import java.util.UUID

import com.raquo.laminar.api.L._
import com.raquo.laminar.nodes.ReactiveHtmlElement
import org.scalajs.dom
import org.scalajs.dom.html
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

  def iconButton(className: String) = button(
    `type` := "button", cls := "close",
    span(cls := className)
  )

  def networkInput[P](available: Set[InTransitPacket[P]], $out: Observer[DeliverBatch[P]]) = {
    def renderDeliverTo(obs: Observer[NetworkCommand])(to: ProcessId, initial: DeliverTo, $changes: Signal[DeliverTo]) = {
      def renderPacket(id: UUID, packet: PacketWithOp, $changes: Signal[PacketWithOp]) = li(
        p(
          s"Deliver ${packet.p.packet.from}--[${packet.p.packet.payload}]-->${packet.p.packet.to}   ",
          button(
            `type` := "button",
            cls := "btn btn-link p-0",
            span(
              cls := "fas fa-long-arrow-alt-down",
              cls <-- $changes.map(p => if (p.op == DeliverOp) "green-text" else ""),
              onClick.mapToValue(DeliverCommand(packet.p.packet)) --> obs
            )
          ),
          // TODO duplicated code
          button(
            `type` := "button",
            cls := "btn btn-link p-0",
            span(
              cls := "fas fa-trash-alt",
              cls <-- $changes.map(p => if (p.op == DropOp) "red-text" else ""),
              onClick.mapToValue(DropCommand(packet.p.packet)) --> obs
            )
          ),
          button(
            `type` := "button",
            cls := "btn btn-link p-0",
            disabled <-- $changes.map(_.op == NoOp),
            span(
              cls := "fas fa-times",
              onClick.mapToValue(ResetCommand(packet.p.packet)) --> obs
            )
          )
        )
      )

      ul(cls := "list-group list-group-flush",
        children <-- $changes.map(_.packets.values.toList).split(_.p.packet.id)(renderPacket)
      )
    }

    object DeliverToBatch {
      def apply(map: List[DeliverTo]): DeliverToBatch = new DeliverToBatch(map.map(dt => dt.processId -> dt).toMap)
    }

    case class DeliverToBatch(delivers: Map[ProcessId, DeliverTo]) {
      def update(f: (DeliverTo, UUID) => DeliverTo)(p: Packet[P]) = copy(delivers + (p.to -> f(delivers(p.to), p.id)))

      def drop = update(_ drop _) _

      def deliver = update(_ deliver _) _

      def reset = update(_ reset _) _

      def ops = delivers.values.foldLeft(Seq.empty[Either[FLLDeliver[P],Drop[P]]])(_ ++ _.ops)

      def clear = copy(delivers.map { case (id,v) => id -> v.clear })

      def values = delivers.values

      def isEmpty = delivers.isEmpty
    }

    sealed trait NetworkCommand
    case class DropCommand(p: Packet[P]) extends NetworkCommand
    case class DeliverCommand(p: Packet[P]) extends NetworkCommand
    case class ResetCommand(p: Packet[P]) extends NetworkCommand

    sealed trait PacketOp
    case object DeliverOp extends PacketOp
    case object DropOp extends PacketOp
    case object NoOp extends PacketOp

    case class PacketWithOp(p: InTransitPacket[P], op: PacketOp)

    object DeliverTo {
      def apply(packets: Set[InTransitPacket[P]]): DeliverTo = new DeliverTo(packets.head.packet.to, packets.map(p => p.packet.id -> PacketWithOp(p, NoOp)).toMap)
    }

    case class DeliverTo(processId: ProcessId, packets: Map[UUID, PacketWithOp]) {
      def update(op: PacketOp)(id: UUID) = copy(packets = packets.map {
        case (`id`, p) => id -> p.copy(op = op)
        case (i, p) => i -> p.copy(op = NoOp)
      })

      def drop = update(DropOp) _

      def deliver = update(DeliverOp) _

      def reset = update(NoOp) _

      def ops = packets.values.collect {
        case PacketWithOp(p, DeliverOp) => Left(p.deliver)
        case PacketWithOp(p, DropOp) => Right(p.drop)
      }

      def clear = packets.keySet.foldLeft(this)(_ reset _)
    }

    val raw = available.groupBy(_.packet.to).map { case (_, packets) => DeliverTo(packets) }.toList
    val batch = Var(DeliverToBatch(raw))

    val commandObs = Observer[NetworkCommand] {
      case DropCommand(p) => batch.update(_.drop(p))
      case DeliverCommand(p) => batch.update(_.deliver(p))
      case ResetCommand(id) => batch.update(_.reset(id))
    }

    div(
      child <-- batch.signal.map(b => if (b.isEmpty) label("Network is empty") else label("")),
      children <-- batch.signal.map(_.values.toList).split(_.processId)(renderDeliverTo(commandObs)),
      button(`type` := "reset", cls := "btn btn-primary", "Next",
        onClick.mapTo(batch.now()).map(b => DeliverBatch(b.ops : _*)) --> $out
      ),
      button(`type` := "button", cls := "btn btn-danger", "Clear",
        //disabled <-- $batch.signal.map(_.ops.isEmpty),
        onClick.mapToValue(()) --> Observer.apply[Unit](_ => batch.update(_.clear))
      )
    )
  }

  sealed trait BatchCommand[+R]

  case object Reset extends BatchCommand[Nothing]

  object AddReq {
    def apply[R](id: ProcessId, req: R): AddReq[R] = new AddReq(id, RequestWrapper(req))
  }

  sealed trait AddCommand[R] extends BatchCommand[R]

  case class AddReq[R](id: ProcessId, r: AddReqInput[R]) extends AddCommand[R]

  sealed trait AddReqInput[+R]

  case class RequestWrapper[R](req: R) extends AddReqInput[R]

  case object CrashP extends AddReqInput[Nothing]

  case class RemoveReq(id: ProcessId) extends BatchCommand[Nothing]

  type Inputs[R] = (List[ProcessId], Observer[AddCommand[R]]) => ReactiveHtmlElement[html.Div]

  def reqBatchInput[Req](inputs: List[Inputs[Req]], processes: List[ProcessId], $obs: Observer[RequestBatch[Req]]) = {
    val $commands = new EventBus[BatchCommand[Req]]
    val $batch: Signal[RequestBatch[Req]] =
      $commands.events.fold(RequestBatch[Req](Map.empty)) {
        case (_, Reset) => RequestBatch[Req](Map.empty)
        case (batch, AddReq(id, RequestWrapper(r))) => batch.add(id, r)
        case (batch, AddReq(id, CrashP)) => batch.crash(id)
        case (batch, RemoveReq(id)) => batch.remove(id)
      }

    def renderBatch = {
      def renderReq[R](to: ProcessId, initial: (ProcessId, ProcessInput[R]), $changes: Signal[(ProcessId, ProcessInput[R])]) = tr(
        th(
          //scope := "",
          to.toString
        ),
        td(
          child <-- $changes.map {
            case (_, ProcessRequest(_, r)) => label(r.toString)
            case (_, Crash(_)) => label("Crash")
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

    div(
      form(
        inputs.map(_.apply(processes, $commands.writer))
      ),
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

  case object Click

  def plus(enabled: Signal[Boolean], obs: Observer[Click.type]) = button(`type` := "button", cls := "close",
    span(
      cls := "fas fa-plus",
      cls <-- enabled.map(e => if (e) "green-text" else "")
    ),
    disabled <-- enabled.map(!_),
    onClick.mapToValue(Click) --> obs
  )

  def plusDiv = (plus _).tupled.andThen(x => div(cls := "col-sm mb-2", x))

  def crash[R] = noPayloadRequest[R](_ => CrashP) _

  def noPayloadRequest[R](f: ProcessId => AddReqInput[R])(processes: List[ProcessId], obs: Observer[AddCommand[R]]): ReactiveHtmlElement[html.Div] = {
    val process: Var[Option[ProcessId]] = Var(None)
    val click = new EventBus[Click.type]
    val d = div(cls := "form-row",
      selectDiv((processes, process.writer, Seq(id := "processId"))),
      plusDiv(process.signal.map(_.isDefined), click.writer)
    )
    click.events.mapTo(process.now().map(id => AddReq(id, f(id))))
      .collect { case Some(r) => r }.addObserver(obs)(d)
    d
  }

  def payloadRequest[R](f: (ProcessId, String) => R)(processes: List[ProcessId], obs: Observer[AddCommand[R]]): ReactiveHtmlElement[html.Div] = {
    val process: Var[Option[ProcessId]] = Var(None)
    val payload: Var[Option[String]] = Var(None)
    val click = new EventBus[Click.type]
    val d = div(cls := "form-row",
      selectDiv((processes, process.writer, Seq(id := "processId"))),
      div(cls := "col-sm mb-2",
        label(forId := "bebPayload", "Payload"),
        input(
          id := "bebProcessId", // TODO estoy repitiendo los ids!
          cls := "form-control",
          inContext { thisNode =>
            @inline def updatePayload = Option(thisNode.ref.value).filterNot(_.isEmpty)

            onMouseOut.stopPropagation.mapTo(updatePayload) --> payload.writer
          }
        )
      ),
      plusDiv(payload.signal.map(_.isDefined), click.writer)
    )
    click.events.mapTo {
      for {
        id <- process.now()
        msg <- payload.now()
      } yield AddReq(id, f(id, msg))
    }.collect { case Some(r) => r }.addObserver(obs)(d)
    d
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

  // IMPROVE
  def selectDiv = (selectProcess _).tupled.andThen(x => div(cls := "col-sm mb-3",
    label(forId := "processId", "Process"),
    x
  ))

}