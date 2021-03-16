name := "Chess"
version := "0.1.0"

scalaVersion := "2.13.4"

addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")

libraryDependencies ++= List(
  "org.typelevel" %% "cats-effect" % "3.0.0-RC2",
  "co.fs2"        %% "fs2-core"    % "3.0-5795280",
  "org.scalatest" %% "scalatest"   % "3.2.2" % "test",
)
