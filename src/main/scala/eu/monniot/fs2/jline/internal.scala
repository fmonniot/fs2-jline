package eu.monniot.fs2.jline

import cats.data.NonEmptyList
import cats.effect.Sync
import com.monovore.decline.Command
import fs2.Stream
import org.jline.reader.{LineReader, LineReaderBuilder}
import org.jline.terminal.{Terminal, TerminalBuilder}

import scala.concurrent.blocking

private[jline] object internal {

  def delay[F[_]: Sync, A](a: => A): Stream[F, A] =
    Stream.eval(Sync[F].delay(a))

  def terminal[F[_]: Sync]: Stream[F, Terminal] = delay(TerminalBuilder.builder()
    .system(true)
    .build())

  def reader[F[_]: Sync](terminal: Terminal): Stream[F, LineReader] = delay(LineReaderBuilder.builder()
    .appName("testing")
    .terminal(terminal)
    .build())

  def readLine[F[_]: Sync](prompt: String, reader: LineReader): Stream[F, NonEmptyList[String]] =
    Stream.eval(Sync[F].delay(blocking(reader.readLine(prompt))))
      .flatMap { line =>
        NonEmptyList.fromList(line.split(" ").toList) match {
          case None => Stream.empty
          case Some(nel) => Stream.emit(nel)
        }
      }

  def findCommand[F[_]: Sync, C](commands: NonEmptyList[Command[C]],
                                 line: NonEmptyList[String]): Stream[F, Command[C]] = {
    commands.find(_.name == line.head) match {
      case None =>
        delay(println(s"Unknown command ${line.head}")) >> Stream.empty
      case Some(cmd) => Stream.emit(cmd)
    }
  }

  def parse[F[_]: Sync, C](cmd: Command[C], args: List[String]): Stream[F, C] = {
    cmd.parse(args).fold(
      error => delay(println(error)) >> Stream.empty,
      c => Stream.emit(c)
    )
  }



}
