package oss.giussi.cappio

object Conditions {

  val NO_ERROR: Option[String] = None

  type Validation[I] = I => Option[String]

  type ProcessesValidation[M <: Mod] = Condition[Set[Process[M]]]

  type IndicationValidation[I] = Condition[List[IndicationFrom[I]]]

  type NetworkValidation[P] = Condition[Network[P]]

  type Condition[I] = I => ConditionResult

  def condition[I](id: Int, description: String, validation: Validation[I]): Condition[I] = new Function1[I, ConditionResult] {
    override def apply(v1: I): ConditionResult = validation(v1) match {
      case None => ConditionResult(id, description, Successful)
      case Some(msg) => ConditionResult(id, description, Error(msg))
    }
  }

  object Validations {
    def ALL_UP[M <: Mod]: Validation[Set[Process[M]]] = processes => {
      //val down = processes.filter(_.status == Down)  // TODO
      val down = processes.filter(_.status == Up)
      if (down.isEmpty) NO_ERROR
      else Some(s"Processes ${down.mkString(",")} has crashed")
    }
  }

  def processes[M <: Mod](v: ProcessesValidation[M])(scheduler: Scheduler[M]) = v(scheduler.processes.values.toSet)

  def network[M <: Mod](v: NetworkValidation[M#Payload])(scheduler: Scheduler[M]) = v(scheduler.network)

  // FIXME sacar el id de aca, de eso deberia encargarse el q las usa
  // FIXME porque necesito pasarle el Mod si no me interesa para la validacion, solo necesito el estado de los procesos
  def ALL_UP[M <: Mod] = condition(1, "All processes should be Up", Validations.ALL_UP[M])
}

sealed trait Result

case object Successful extends Result

case class Error(msg: String) extends Result

case class ConditionResult(id: Int, description: String, result: Result) {
  def ok = result == Successful
}
