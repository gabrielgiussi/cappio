package oss.giussi.scalorm

object CMIErrorCode {
  def apply(code: Int): CMIErrorCode = code match {
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

  def code(error: CMIErrorCode): Int = error match {
    case NoError => 0
    case GeneralException => 101
    case InvalidArgumentError => 201
    case CannotHaveChildrenError => 202
    case NotAnArrayError => 203
    case NotInitializedError => 301
    case NotImplementedError => 401
    case InvalidSetValueError => 402
    case ReadOnlyElementError => 403
    case WriteOnlyElementError => 404
    case IncorrectDataType => 405
    case UnknownError(_) => -1
  }
}

sealed trait CMIErrorCode

case object NoError extends CMIErrorCode

case object GeneralException extends CMIErrorCode

case object InvalidArgumentError extends CMIErrorCode

case object CannotHaveChildrenError extends CMIErrorCode

case object NotAnArrayError extends CMIErrorCode

case object NotInitializedError extends CMIErrorCode

case object NotImplementedError extends CMIErrorCode

case object InvalidSetValueError extends CMIErrorCode

case object ReadOnlyElementError extends CMIErrorCode

case object WriteOnlyElementError extends CMIErrorCode

case object IncorrectDataType extends CMIErrorCode

case class UnknownError(code: Int) extends CMIErrorCode