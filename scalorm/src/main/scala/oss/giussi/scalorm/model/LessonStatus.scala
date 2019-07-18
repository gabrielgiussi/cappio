package oss.giussi.scalorm.model

object LessonStatus {

  def apply(s: String): LessonStatus = s match {
    case "passed" => Passed
    case "completed" => Completed
    case "failed" => Failed
    case "incomplete" => Incomplete
    case "browsed" => Browsed
    case "not attempted" => NotAttempted
  }

  def value(status: LessonStatus): String = status match {
    case Passed => "passed"
    case Completed => "completed"
    case Failed => "failed"
    case Incomplete => "incomplete"
    case Browsed => "browsed"
    case NotAttempted => "not attempted"
  }
}

sealed trait LessonStatus
case object Passed extends LessonStatus
case object Completed extends LessonStatus
case object Failed extends LessonStatus
case object Incomplete extends LessonStatus
case object Browsed extends LessonStatus
case object NotAttempted extends LessonStatus