package oss.giussi.scalorm

object Scalorm {

  def getLastError(): ScormError = ScormError(Scorm.getLastError())
}
