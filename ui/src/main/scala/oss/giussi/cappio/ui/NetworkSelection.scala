package oss.giussi.cappio.ui

import java.util.UUID

import com.raquo.laminar.api.L._
import oss.giussi.cappio.Network.InTransitPacket
import oss.giussi.cappio.{DeliverBatch, Drop, FLLDeliver, Packet, ProcessId}

object NetworkSelection {

  import ShowSyntax._

  def networkInput[P : Show](available: Set[InTransitPacket[P]], $out: Observer[DeliverBatch[P]]) = {
    def renderDeliverTo(obs: Observer[NetworkCommand])(to: ProcessId, initial: DeliverTo, $changes: Signal[DeliverTo]) = {
      def renderPacket(id: UUID, packet: PacketWithOp, $changes: Signal[PacketWithOp]) = li(
        cls := "list-group-item p-0",
        div(
          s"Deliver ${packet.p.packet.from}--[${packet.p.packet.payload.show}]-->${packet.p.packet.to}   ",
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
            disabled := (packet.p.packet.from == packet.p.packet.to),
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

      def drop = update(DropOp)  _

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

    val a_div = if (available.isEmpty) div(h2("No hay paquetes para entregar"))
    else div(
      children <-- batch.signal.map(_.values.toList).split(_.processId)(renderDeliverTo(commandObs))
    )

    div(
      cls := "text-center",
      a_div,
      div(
        button(`type` := "reset", cls := "btn btn-primary", "Siguiente",
          disabled <-- batch.signal.map(_.ops.isEmpty && available.nonEmpty),
          onClick.mapTo(batch.now()).map(b => DeliverBatch(b.ops : _*)) --> $out
        ),
        button(`type` := "button", cls := "btn btn-danger", "Limpiar",
          disabled <-- batch.signal.map(_.ops.isEmpty),
          onClick.mapToValue(()) --> Observer.apply[Unit](_ => batch.update(_.clear))
        )
      )
    )
  }

}
