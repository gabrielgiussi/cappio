package oss.giussi.cappio.oa

import oss.giussi.scalorm.Scorm

object App {

  def main(args: Array[String]): Unit = {
    if (Scorm.initialize()){
      println("Ok")
      //println(Level.level.map(_ * 2))
    }
    else {
      println("No API found")
    }
  }

}
