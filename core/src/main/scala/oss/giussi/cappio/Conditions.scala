package oss.giussi.cappio

object Conditions {

  type ProcessValidation[M <: Mod] = Condition[Process[M]]

  type ProcessesValidation[M <: Mod] = Condition[Set[Process[M]]]

  type IndicationValidation[I] = Condition[Set[IndicationFrom[I]]]

  type NetworkValidation[P] = Condition[Network[P]]

  type StatesValidation[M <: Mod] = Condition[Map[ProcessId,M#State]]

  type StateValidation[M <: Mod] = Condition[M#State]

  type Condition[I] = I => Result

  case class ConditionWithDescription[I](short: String, description: String, f: Condition[I]) extends Function1[I,ConditionResult] {
    override def apply(v1: I): ConditionResult = ConditionResult(short,description, f.apply(v1))
  }

  def condition[I](short:String, description: String, validation: Condition[I]): ConditionWithDescription[I] = ConditionWithDescription(short,description, validation)

  object Validations {
    def ALL_UP[M <: Mod]: Condition[Set[Process[M]]] = processes => {
      val down = processes.filter(_.status == Down).map(_.id)
      if (down.isEmpty) Successful
      else Error(s"Processes [${down.mkString(",")}] has crashed")
    }
  }

}

sealed trait Result

case object Successful extends Result

case class Error(msg: String) extends Result

case class ConditionResult(short: String, description: String, result: Result) {
  def ok = result == Successful
}
