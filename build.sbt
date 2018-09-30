import sbt.Keys.{dependencyOverrides, libraryDependencies}

ThisBuild / organization := "oss.ggiussi"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "2.12.6"


lazy val core = (project in file("core"))
  .settings(
    // other settings
  ) enablePlugins (ScalaJSPlugin)

val react = "1.2.3" // TODO move inside ui

lazy val ui = (project in file("ui"))
  .settings(
    scalaJSUseMainModuleInitializer := true,
    mainClass in Compile := Some("oss.ggiussi.cappio.ui.App"),
    // other settings
    libraryDependencies ++= Seq(
      "com.github.japgolly.scalajs-react" %%% "core" % react,
      "com.github.japgolly.scalajs-react" %%% "extra" % react
    ),
    jsDependencies ++= Seq(

      "org.webjars.npm" % "react" % "16.2.0"
        / "umd/react.development.js"
        minified "umd/react.production.min.js"
        commonJSName "React",

      "org.webjars.npm" % "react-dom" % "16.2.0"
        / "umd/react-dom.development.js"
        minified "umd/react-dom.production.min.js"
        dependsOn "umd/react.development.js"
        commonJSName "ReactDOM",

      "org.webjars.npm" % "react-dom" % "16.2.0"
        / "umd/react-dom-server.browser.development.js"
        minified "umd/react-dom-server.browser.production.min.js"
        dependsOn "umd/react-dom.development.js"
        commonJSName "ReactDOMServer"),

    dependencyOverrides += "org.webjars.npm" % "js-tokens" % "3.0.2"
  )
  .enablePlugins(ScalaJSPlugin)
  .dependsOn(core)