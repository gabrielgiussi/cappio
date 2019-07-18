package oss.giussi.scalorm

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

@JSImport("@gamestdio/scorm", "scorm")
@js.native
object Scorm extends js.Object {

    // https://scorm.com/scorm-explained/technical-scorm/run-time/run-time-reference/
  def initialize(): Boolean = js.native

  def getLastError(): Int = js.native

  def terminate(): Boolean = js.native

  def commit(): Boolean = js.native

  // TODO errorCode int or string?
  def getErrorString(errorCode: Int): String = js.native

  def getDiagnostic(errorCode: Int): String = js.native

  def set(element: String, value: String): Boolean = js.native

  def get(element: String): String = js.native

  def isActive: Boolean = js.native

  def version: String = js.native

  //def configure()

  // def status()

}
