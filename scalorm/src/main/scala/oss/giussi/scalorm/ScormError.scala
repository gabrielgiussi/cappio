package oss.giussi.scalorm

object ScormError {
  def apply(code: Int): ScormError = code match {
    case 0 => NoError
    case 101 => GeneralException
    case 201 => InvalidArgumentError
    case 202 => CannotHaveChildrenError
    case 203 => NotAnArrayError
    case 301 => NotInitializedError
    case 401 => NotImplementedError
    case 402 => InvalidSetValueError
    case 403 => ReadOnlyElementError
    case 404 => WriteOnlyElementError
    case 405 => IncorrectDataType
    case x => UnknownError(x)

  }
}

sealed trait ScormError

case object NoError extends ScormError

case object GeneralException extends ScormError

case object InvalidArgumentError extends ScormError

case object CannotHaveChildrenError extends ScormError

case object NotAnArrayError extends ScormError

case object NotInitializedError extends ScormError

case object NotImplementedError extends ScormError

case object InvalidSetValueError extends ScormError

case object ReadOnlyElementError extends ScormError

case object WriteOnlyElementError extends ScormError

case object IncorrectDataType extends ScormError

case class UnknownError(code: Int) extends ScormError