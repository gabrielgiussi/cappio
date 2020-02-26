package oss.giussi.cappio.ui

import java.util.UUID

import com.raquo.laminar.api.L._
import com.raquo.laminar.nodes.ReactiveHtmlElement
import org.scalajs.dom
import org.scalajs.dom.html
import oss.giussi.cappio.Network.InTransitPacket
import oss.giussi.cappio._
import oss.giussi.cappio.ui.levels.{PredefinedAction, RequestBatch}
import ShowSyntax._

object ActionSelection {

  type Select = ReactiveHtmlElement[dom.html.Select]

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

  type Inputs[R] = (List[ProcessId], Observer[AddCommand[R]]) => ReactiveHtmlElement[html.Div] // FIXME refactor this



  def reqPredefined[Req : Show](predefined: Map[ProcessId, ProcessInput[Req]])(inputs: List[Inputs[Req]], processes: List[ProcessId], $obs: Observer[RequestBatch[Req]]) = {
    val availableProcesses = processes.filterNot(predefined.keySet.contains)

    val $commands = new EventBus[BatchCommand[Req]]
    val initialBatch = RequestBatch[Req](predefined)
    val $batch: Signal[RequestBatch[Req]] =
      $commands.events.fold(initialBatch) {
        case (_, Reset) => initialBatch
        case (batch, AddReq(id, RequestWrapper(r))) => batch.add(id, r)
        case (batch, AddReq(id, CrashP)) => batch.crash(id)
        case (batch, RemoveReq(id)) => batch.remove(id)
      }

    def renderBatch = {
      def renderReq(to: ProcessId, initial: (ProcessId, ProcessInput[Req]), $changes: Signal[(ProcessId, ProcessInput[Req])]) = tr(
        th(
          //scope := "",
          to.toString
        ),
        td(
          child <-- $changes.map {
            case (_, ProcessRequest(_, r, _)) => label(r.show)
            case (_, Crash(_, _)) => label("Crash")
          }
        ),
        td(
          button(`type` := "button", cls := "close",
            disabled := initial._2.predefined,
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
        inputs.map(_.apply(availableProcesses, $commands.writer))
      ),
      renderBatch,
      div(cls := "text-center",
        input(`type` := "submit", cls := "btn btn-primary", value := "Siguiente", // TODO input or button?
          inContext { thisNode =>
            val a = $batch.observe(thisNode)
            onClick.preventDefault.mapTo(a.now()) --> $obs
          }
        ),
        input(`type` := "reset", cls := "btn btn-danger", value := "Limpiar",
          disabled <-- $batch.map(_.requests.isEmpty),
          onClick.preventDefault.mapToValue(Reset) --> $commands
        )
      )
    )
  }


  def reqBatchInput[Req : Show](inputs: List[Inputs[Req]], processes: List[ProcessId], $obs: Observer[RequestBatch[Req]]) = {
    val $commands = new EventBus[BatchCommand[Req]]
    val $batch: Signal[RequestBatch[Req]] =
      $commands.events.fold(RequestBatch[Req](Map.empty)) {
        case (_, Reset) => RequestBatch[Req](Map.empty)
        case (batch, AddReq(id, RequestWrapper(r))) => batch.add(id, r)
        case (batch, AddReq(id, CrashP)) => batch.crash(id)
        case (batch, RemoveReq(id)) => batch.remove(id)
      }

    def renderBatch = {
      def renderReq(to: ProcessId, initial: (ProcessId, ProcessInput[Req]), $changes: Signal[(ProcessId, ProcessInput[Req])]) = tr(
        th(
          //scope := "",
          to.toString
        ),
        td(
          child <-- $changes.map {
            case (_, ProcessRequest(_, r, _)) => label(r.show)
            case (_, Crash(_, _)) => label("Crash")
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
      div(cls := "text-center",
        input(`type` := "submit", cls := "btn btn-primary", value := "Siguiente", // TODO input or button?
          inContext { thisNode =>
            val a = $batch.observe(thisNode)
            onClick.preventDefault.mapTo(a.now()) --> $obs
          }
        ),
        input(`type` := "reset", cls := "btn btn-danger", value := "Limpiar",
          disabled <-- $batch.map(_.requests.isEmpty),
          onClick.preventDefault.mapToValue(Reset) --> $commands
        )
      )
    )
  }

  case object Click

  def plus(enabled: Signal[Boolean], obs: Observer[Click.type]) = button(`type` := "button", cls := "btn btn-link p-1",
    span(
      cls := "fas fa-plus",
      cls <-- enabled.map(e => if (e) "green-text" else "")
    ),
    disabled <-- enabled.map(!_),
    onClick.mapToValue(Click) --> obs
  )

  def plusDiv = (plus _).tupled.andThen(x => div(cls := "col-sm-2", x))

  def crash[R] = noPayloadRequest[R]("Terminar")(_ => CrashP) _

  def noPayloadRequest[R](description: String)(f: ProcessId => AddReqInput[R])(processes: List[ProcessId], obs: Observer[AddCommand[R]]): ReactiveHtmlElement[html.Div] = {
    val process: Var[Option[ProcessId]] = Var(None)
    val click = new EventBus[Click.type]
    val d = div(cls := "form-row",
      div(cls := "col-sm-3 text-center", label(description)),
      selectDiv("col-sm-6")((processes, process.writer, Seq.empty)),
      plusDiv(process.signal.map(_.isDefined), click.writer)
    )
    click.events.mapTo(process.now().map(id => AddReq(id, f(id))))
      .collect { case Some(r) => r }.addObserver(obs)(d)
    d
  }

  // TODO I never seen code as ugly as this!
  def payloadRequest[R](description: String)(f: (ProcessId, String) => R)(processes: List[ProcessId], obs: Observer[AddCommand[R]]): ReactiveHtmlElement[html.Div] = {
    val process: Var[Option[ProcessId]] = Var(None)
    val payload: Var[Option[String]] = Var(None)
    val click = new EventBus[Click.type]
    val d = div(cls := "form-row",
      div(cls := "col-sm-3 text-center", label(description)),
      selectDiv("col-sm-3")((processes, process.writer, Seq.empty)),
      div(cls := "col-sm-3",
        //label(forId := "bebPayload", "Payload"),
        input(
          cls := "form-control",
          inContext { thisNode =>
            @inline def updatePayload = Option(thisNode.ref.value).filterNot(_.isEmpty)

            onKeyUp.stopPropagation.mapTo(updatePayload) --> payload.writer // there is some way to throttle events in airstream?
          }
        )
      ),
      plusDiv(payload.signal.combineWith(process.signal).map { case (pd,ps) => ps.isDefined && pd.isDefined }, click.writer)
    )
    click.events.mapTo {
      for {
        id <- process.now()
        msg <- payload.now()
      } yield AddReq(id, f(id, msg))
    }.collect { case Some(r) => r }.addObserver(obs)(d)
    d
  }

  def fromToPayloadRequest[R](description: String)(f: (ProcessId,ProcessId, String) => R)(processes: List[ProcessId], obs: Observer[AddCommand[R]]) = {
    val from: Var[Option[ProcessId]] = Var(None)
    val to: Var[Option[ProcessId]] = Var(None)
    val payload: Var[Option[String]] = Var(None)
    val click = new EventBus[Click.type]
    val d = div(cls := "form-row",
      div(cls := "col-sm-2 text-center", label(description)),
      selectDiv("col-sm-3")((processes, from.writer, Seq.empty)),
      selectDiv("col-sm-3")((processes, to.writer, Seq.empty)),
      div(cls := "col-sm-2",
        //label(forId := "bebPayload", "Payload"),
        input(
          cls := "form-control",
          inContext { thisNode =>
            @inline def updatePayload = Option(thisNode.ref.value).filterNot(_.isEmpty)

            onKeyUp.stopPropagation.mapTo(updatePayload) --> payload.writer // there is some way to throttle events in airstream?
          }
        )
      ),
      plusDiv(payload.signal.combineWith(from.signal).combineWith(to.signal).map { case ((from,to),ps) => ps.isDefined && from.isDefined && to.isDefined }, click.writer)
    )
    click.events.mapTo {
      for {
        fid <- from.now()
        tid <- to.now()
        msg <- payload.now()
        if fid != tid
      } yield AddReq(fid, f(fid,tid, msg))
    }.collect { case Some(r) => r }.addObserver(obs)(d)
    d
  }


  def selectProcess(processes: List[ProcessId], obs: Observer[Option[ProcessId]], modifiers: Modifier[Select]*) = {
    select(
      cls := "browser-default custom-select mb-2",
      inContext(thisNode => onInput.mapTo(thisNode.ref.value).map(Option(_).filterNot(_ == "unset").map(_.toInt).map(ProcessId)) --> obs),
      option(
        "-",
        value := "unset"
      ) :: processes.map(p => option(
        p.id.toString,
        value := p.id.toString
      )),
      modifiers
    )
  }

  // IMPROVE
  def selectDiv(c: String) = (selectProcess _).tupled.andThen(x => div(cls := c,
    //label(forId := "processId", "Process"),
    x
  ))

}