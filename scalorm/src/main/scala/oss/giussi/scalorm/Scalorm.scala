package oss.giussi.scalorm

import oss.giussi.scalorm.model.LessonStatus

// https://scorm.com/scorm-explained/technical-scorm/run-time/run-time-reference/#section-2
object Scalorm {

  import CMIElement._

  def getLastError(): CMIErrorCode = CMIErrorCode(Scorm.getLastError())

  def getErrorString(error: CMIErrorCode): String = Scorm.getErrorString(CMIErrorCode.code(error))

  def getDiagnostic(error: CMIErrorCode): String = Scorm.getDiagnostic(CMIErrorCode.code(error))

  def commit(): Boolean = Scorm.commit()

  def initialize(): Boolean = Scorm.initialize()

  def finish(): Boolean = Scorm.terminate()

  def getStudentId(): String = Scorm.get(Core.STUDENT_ID)

  def getStudentName: String = Scorm.get(Core.STUDENT_NAME)

  def getLessonLocation(): String = Scorm.get(Core.LESSON_LOCATION)

  def setLessonLocation(value: String): Boolean = Scorm.set(Core.LESSON_LOCATION,value)

  def getLessonStatus(): LessonStatus = LessonStatus(Scorm.get(Core.LESSON_STATUS))

  def setLessonStatus(value: LessonStatus): Boolean = Scorm.set(Core.LESSON_STATUS,LessonStatus.value(value))

  def getLaunchData(): String = Scorm.get(LAUNCH_DATA)


}
