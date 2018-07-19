lazy val CatsEffectVersion = "1.0.0-RC"
lazy val Fs2Version        = "1.0.0-M1"
lazy val JLineVersion      = "3.9.0"

lazy val ScalaTestVersion  = "3.0.3"
lazy val ScalaCheckVersion = "1.13.4"

lazy val `fs2-jline` = (project in file("."))
  .settings(
    organization := "eu.monniot.fs2.jline",
    name := "fs2-jline",
    version := "0.0.1-SNAPSHOT",
    scalaVersion := "2.12.6",
    scalacOptions := Seq(
      "-deprecation",
      "-encoding",
      "UTF-8",
      "-feature",
      "-language:existentials",
      "-language:higherKinds",
      "-Ypartial-unification"
    ),
    libraryDependencies ++= Seq(
      "org.typelevel"   %% "cats-effect"         % CatsEffectVersion,
      "co.fs2"          %% "fs2-core"            % Fs2Version,
      "co.fs2"          %% "fs2-io"              % Fs2Version,

      "org.jline"       %  "jline"               % "3.9.0",
      "com.monovore"    %% "decline"             % "0.4.2",

      "org.scalatest"   %% "scalatest"           % ScalaTestVersion  % Test,
      "org.scalacheck"  %% "scalacheck"          % ScalaCheckVersion % Test
    ),
    mainClass in assembly := Some("eu.monniot.fs2.jline.SafeTerminal"),
  )

