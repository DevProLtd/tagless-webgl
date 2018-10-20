enablePlugins(ScalaJSPlugin)

name := "tagless-webgl"

version := "1.0"

scalaVersion := "2.12.4"
scalacOptions += "-Ypartial-unification"

libraryDependencies ++= Seq(
  "org.typelevel" %%% "cats-core"   % "1.3.1",
  "org.typelevel" %%% "cats-effect" % "1.0.0",
  "org.scala-js" %%% "scalajs-dom"  % "0.9.6",
  "co.fs2" %%% "fs2-core"           % "1.0.0-M5",
  "io.monix" %%% "monix"            % "3.0.0-RC1"
)

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.7")

scalaJSUseMainModuleInitializer := true

jsEnv := new org.scalajs.jsenv.jsdomnodejs.JSDOMNodeJSEnv()
