package oss.giussi.cappio.ui.core

import oss.giussi.cappio.Conditions._
import oss.giussi.cappio.ui.levels.{PredefinedAction, Snapshot}
import oss.giussi.cappio.{Error, Mod, ProcessId, ProcessRequest, Successful}

object LevelConditions {

  private object Projections {

    def processes[M <: Mod] = (s: Snapshot[M]) => s.step.scheduler.processes.values.toSet

    def process[M <: Mod](id: ProcessId) = processes[M].andThen(_.find(_.id == id).get)

    def network[M <: Mod] = (s: Snapshot[M]) => s.step.scheduler.network

    def indications[M <: Mod] = (s: Snapshot[M]) => s.indications

    def states[M <: Mod] = processes[M].andThen(_.map(p => p.id -> p.stack.state).toMap)

    def state[M <: Mod](id: ProcessId) = states[M].andThen(_(id))
  }

  def process[M <: Mod](id: ProcessId)(v: ProcessValidation[M]) = Projections.process(id).andThen(v)

  def processes[M <: Mod](v: ProcessesValidation[M]) = Projections.processes.andThen(v)

  def network[M <: Mod](v: NetworkValidation[M#Payload]) = Projections.network.andThen(v)

  def indications[M <: Mod](v: IndicationValidation[M#Ind]) = Projections.indications.andThen(v)

  def states[M <: Mod](v: StatesValidation[M]) = Projections.states.andThen(v)

  def state[M <: Mod](id: ProcessId)(v: StateValidation[M]) = Projections.state(id).andThen(v)

  def ALL_UP[M <: Mod]: ConditionWithDescription[Snapshot[M]] = condition("All Up", "All processes should be Up", processes(Validations.ALL_UP[M]))

  def boundedDelay[M <: Mod](delay: Int): ConditionWithDescription[Snapshot[M]] = condition("TODO", "TODO", network(n => if (n.badPackets(delay).isEmpty) Successful else Error("")))

  def predefinedActions[M <: Mod](actions: Set[PredefinedAction[M#Req]], toRequest: PredefinedAction[M#Req] => Action): ConditionWithDescription[Snapshot[M]] = condition("Predefined", "TODO", snapshot => {
    val shouldBeenTriggered: Set[Action] = actions.filter(_.index.i < snapshot.index.i).map(toRequest)
    val requests: Set[Action] = snapshot.actions.collect {
      case r@Request(_, _, _, _, false, _) => r.copy(predefined = true) // this copy is a (ugly) hack for the == with the shouldBeenTriggered to work
    }.toSet
    val notPresent = (shouldBeenTriggered diff requests)
    if (notPresent.isEmpty) Successful else Error("Algunas de los requests requeridos no pudieron ser ejecutados porque el proceso no se encontraba en ejecucion") // TODO show wich actions
  })


}
