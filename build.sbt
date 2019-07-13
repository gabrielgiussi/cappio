
ThisBuild / organization := "oss.ggiussi"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "2.12.8"

lazy val commonSettings = Seq(
  libraryDependencies += "org.scalatest" %%% "scalatest" % "3.0.5" % Test
)

lazy val cappio = project.in(file("."))
  .aggregate(core, ui)

lazy val core = (project in file("core"))
  .settings(
    commonSettings,
    libraryDependencies ++= Seq(
    )
  ) enablePlugins (ScalaJSPlugin) // disabled for testing


lazy val ui = (project in file("ui"))
  .settings(
    //resolvers += "jitpack" at "https://jitpack.io",
    scalaJSUseMainModuleInitializer := true,
    //scalaJSUseMainModuleInitializer in Test := false,

    //mainClass in Compile := Some("oss.giussi.cappio.ui.SampleMain"),
    // other settings
    libraryDependencies ++= Seq(
      //"com.github.outwatch" % "outwatch" % "676f94ab57"
      "com.raquo" %%% "laminar" % "0.7"
    ),

  )

  .enablePlugins(ScalaJSPlugin)
  .dependsOn(core)

