package eu.monniot.fs2.jline

import cats.effect.IO
import eu.monniot.fs2.jline.SafeTerminal.{app, commands}
import fs2.{Stream, StreamApp}

import scala.concurrent.ExecutionContext.Implicits.global


object main extends StreamApp[IO] {
  override def stream(args: List[String], requestShutdown: IO[Unit]): Stream[IO, StreamApp.ExitCode] =
    Fs2JLine(commands)(app[IO])
}
