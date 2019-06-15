package oss.giussi.cappio.ui

import java.util.UUID

import com.raquo.laminar.api.L._
import oss.giussi.cappio.ui.core.{Action, Crashed, Delivered, Dropped, Index, Indication, Request, Undelivered}
import oss.giussi.cappio.{NextStateScheduler, ProcessId, ProcessStatus, Processes, Scheduler, Up}

object Levels {

  def level1 = LevelFactory(1, () => LevelImpl(1, Processes(Set(ProcessId(0), ProcessId(1), ProcessId(2)))))

  def level2 = LevelFactory(2, () => LevelImpl(2, Processes((0 to 10).map(ProcessId).toSet)))

  // FIXME esto es por ahora para que siempre genere un nuevo nivel porque las signal ya van a haber cambiado el valor initial
  val levels: List[LevelFactory] = List(level1,level2)

}

case class LevelFactory(x: Int, f: () => Level)

trait Level {

  val x: Int

  val processes: Processes

  val $actions: EventStream[List[Action]]

  def diagram: Div = Diagram(processes, $actions)

  def actionSelection: Div

  // una lista de div por como lo quiero mostrar usando las tarjetas, sino que se vea como un div solo con cards adentro
  // la logica para actualizar los estados va a estar adentro, aunque tambien es comun a todos los niveles, lo que tienen
  // que definir es como mostrar un state!, dsp va a ser un split por processId
  // tal vez tiene mas sentido que provea un stream de List[(ProcessId,Div)]
  def states: Div

}

case class LevelImpl(x: Int, processes: Processes) extends AbstractLevel[Unit,Unit,Unit](x,processes) {

  override val $states = new EventBus[List[ProcessState]].events.toSignal(processes.ids.map(id => ProcessState(id,(),Up)).toList)

}

abstract class AbstractLevel[R, S, I](x: Int, processes: Processes) extends Level {

  case class ProcessState(id: ProcessId, state: S, status: ProcessStatus)

  //val $requests = new EventBus[R]

  /*

  val snapshotsBus = new EventBus[Snapshot]
  val requests = eventBus.events
  val $scheduler = new EventBus[Scheduler[R,S,I]].events.toSignal(scheduler)
    .changes
    .combineWith(requests)
    .map {
      case (sch,req) =>
        val NextStateScheduler(indications,scheduler) = sch.request(???)
    }

   */

  val $states: Signal[List[ProcessState]]

  val uuid = UUID.randomUUID()

  val actions: List[Action] = List(
    Undelivered(ProcessId(0), ProcessId(2), uuid, "", Index(0)),
    Delivered(ProcessId(0), ProcessId(2), uuid, "", Index(0), Index(2)),
    Delivered(ProcessId(1), ProcessId(0), UUID.randomUUID(), "", Index(3), Index(4)),
    Delivered(ProcessId(2), ProcessId(0), UUID.randomUUID(), "", Index(6), Index(9)),
    Crashed(ProcessId(2), Index(5)),
    Request(ProcessId(0), Index(6), ""),
    Indication(ProcessId(1), Index(20), ""),
    Dropped(ProcessId(0), ProcessId(1), UUID.randomUUID(), "", Index(25), Index(27))
  )

  val clicks = new EventBus[String]

  override val $actions: EventStream[List[Action]] = clicks.events.fold(0)((acc, command) => if (command == "Next") acc + 1 else acc - 1).map(i => actions.take(i)
    .groupBy(_.id).map(_._2.last).toList
  ).changes


  override def actionSelection: Div = {
    val prevButton = button(
      "Prev",
      onClick.preventDefault.mapToValue("Prev") --> clicks
    )
    val nextButton = button(
      "Next",
      onClick.preventDefault.mapToValue("Next") --> clicks
    )
    div(prevButton, nextButton)
  }

  def renderState(id: ProcessId, initial: ProcessState, $states: Signal[ProcessState]) = div(
    cls := "col-md-4 mb-4",
    div(
      cls := "card",
      div(
        cls := "card-body",
        renderStateI(id,initial,$states)
      )
    )
  )

  def renderStateI(id: ProcessId, initial: ProcessState, $states: Signal[ProcessState]): Modifier[Div] = id.toString

  def states: Div = div(
    cls := "row wow fadeIn",
    children <-- $states.split(_.id)(renderState)
  )



}