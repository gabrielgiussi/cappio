
ThisBuild / organization := "oss.giussi"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "2.12.8"

lazy val commonSettings = Seq(
  libraryDependencies += "org.scalatest" %%% "scalatest" % "3.0.5" % Test
)

lazy val cappio = project.in(file("."))
  .aggregate(core, ui, scalorm, oa, spa)

lazy val scalorm = (project in file("scalorm"))
  .settings(npmDependencies in Compile += "@gamestdio/scorm" -> "0.1.3")
  .enablePlugins(ScalaJSPlugin, ScalaJSBundlerPlugin)

lazy val oa = (project in file("oa"))
  .enablePlugins(ScalaJSPlugin, ScalaJSBundlerPlugin, UniversalPlugin)
  .settings(
    scalaJSUseMainModuleInitializer := true,
    maintainer := "gabrielgiussi@gmail.com",
    topLevelDirectory := None,
    packageName in Universal  := "cappio-scorm-12",
    mappings.in(Universal) ++= webpack.in(Compile, fullOptJS).value.map { f =>
      f.data -> s"assets/${f.data.getName()}"
    }
  )
  .dependsOn(ui, scalorm)

lazy val spa = (project in file("spa"))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    scalaJSUseMainModuleInitializer := true
  )
  .enablePlugins(ScalaJSPlugin)
  .dependsOn(ui)

lazy val core = (project in file("core"))
  .settings(
    commonSettings,
    libraryDependencies +=  "com.chuusai" %%% "shapeless" % "2.3.3"
  )
  .enablePlugins(ScalaJSPlugin) // disabled for testing


lazy val ui = (project in file("ui"))
  .settings(
    commonSettings,
    libraryDependencies ++= Seq(
      "com.raquo" %%% "laminar" % "0.7"
    )
  )
  .enablePlugins(ScalaJSPlugin)
  .dependsOn(core)

