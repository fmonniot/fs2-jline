package eu.monniot.fs2.jline.demo

import cats.effect.IO
import eu.monniot.fs2.jline
import fs2.{Stream, StreamApp}

import scala.concurrent.ExecutionContext.Implicits.global


object main {

  val app = new StreamApp[IO] {
    override def stream(args: List[String], requestShutdown: IO[Unit]): Stream[IO, StreamApp.ExitCode] =
      jline.simple(SafeTerminal.commands, IO(0))(SafeTerminal[IO])
  }

  def main(args: Array[String]): Unit = {
    if(args.headOption.contains("java")) {
      UnsafeTerminal()
    } else {
      app.main(args)
    }
  }
}
