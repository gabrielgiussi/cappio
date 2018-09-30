name := "cappio"

version := "0.1"

scalaVersion := "2.12.6"

enablePlugins(ScalaJSPlugin)

scalaJSUseMainModuleInitializer := true

mainClass in Compile := Some("oss.ggiussi.cappio.ui.App")

val react = "1.2.3"

libraryDependencies += "com.github.japgolly.scalajs-react" %%% "core" % react
libraryDependencies += "com.github.japgolly.scalajs-react" %%% "extra" % react

jsDependencies ++= Seq(

  "org.webjars.npm" % "react" % "16.2.0"
    /        "umd/react.development.js"
    minified "umd/react.production.min.js"
    commonJSName "React",

  "org.webjars.npm" % "react-dom" % "16.2.0"
    /         "umd/react-dom.development.js"
    minified  "umd/react-dom.production.min.js"
    dependsOn "umd/react.development.js"
    commonJSName "ReactDOM",

  "org.webjars.npm" % "react-dom" % "16.2.0"
    /         "umd/react-dom-server.browser.development.js"
    minified  "umd/react-dom-server.browser.production.min.js"
    dependsOn "umd/react-dom.development.js"
    commonJSName "ReactDOMServer")

dependencyOverrides += "org.webjars.npm" % "js-tokens" % "3.0.2"