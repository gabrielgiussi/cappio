package oss.giussi.cappio

object Conditions {

  val NO_ERROR: Option[String] = None

  type Validation[I] = I => Option[String]

  type ProcessesValidation[M <: Mod] = Condition[Set[Process[M]]]

  type IndicationValidation[I] = Condition[Set[IndicationFrom[I]]]

  type NetworkValidation[P] = Condition[Network[P]]

  type Condition[I] = I => ConditionResult

  def condition[I](short:String, description: String, validation: Validation[I]): Condition[I] = new Function1[I, ConditionResult] {
    override def apply(v1: I): ConditionResult = validation(v1) match {
      case None => ConditionResult(short, description, Successful)
      case Some(msg) => ConditionResult(short, description, Error(msg))
    }
  }

  object Validations {
    def ALL_UP[M <: Mod]: Validation[Set[Process[M]]] = processes => {
      val down = processes.filter(_.status == Down).map(_.id)
      if (down.isEmpty) NO_ERROR
      else Some(s"Processes [${down.mkString(",")}] has crashed")
    }
  }

  // FIXME porque necesito pasarle el Mod si no me interesa para la validacion, solo necesito el estado de los procesos
  def ALL_UP[M <: Mod] = condition("All Up", "All processes should be Up", Validations.ALL_UP[M])

}

sealed trait Result

case object Successful extends Result

case class Error(msg: String) extends Result

case class ConditionResult(short: String, description: String, result: Result) {
  def ok = result == Successful
}
