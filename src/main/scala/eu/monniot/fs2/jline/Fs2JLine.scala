package eu.monniot.fs2.jline

import cats.data.NonEmptyList
import cats.effect.Sync
import com.monovore.decline.{Command, Opts}
import eu.monniot.fs2.jline.Fs2JLine.builtins
import eu.monniot.fs2.jline.Fs2JLine.builtins.BuiltInCommand
import eu.monniot.fs2.jline.SafeTerminal.Add
import fs2.Stream
import fs2.StreamApp.ExitCode
import org.jline.reader.{LineReader, LineReaderBuilder}
import org.jline.terminal.{Terminal, TerminalBuilder}

import scala.concurrent.blocking

object Fs2JLine {
  def apply[F[_]: Sync, C](availableCommands: NonEmptyList[Command[C]], defaultPrompt: String = ">")
                           (f: C => Stream[F, Unit]): Stream[F, ExitCode] =
    new Fs2JLine[F, C] {
      override val commands = availableCommands

      override def onCommand(c: C) = f(c)

      override def prompt = defaultPrompt
    }.runnableStream


  object builtins {

    sealed trait BuiltInCommand

    case object Quit extends BuiltInCommand

    case object Help extends BuiltInCommand

    val commands: NonEmptyList[Command[BuiltInCommand]] = NonEmptyList.of(
      Command("quit", "Quit this shell")(Opts.unit.map(_ => Quit)),
      Command("q", "Quit this shell")(Opts.unit.map(_ => Quit)),
      Command("help", "Display the basic usage of this shell")(Opts.unit.map(_ => Quit)),
      Command("help", "Display the basic usage of this shell")(Opts.unit.map(_ => Quit)),
    )

    def onBuiltinCommand[F[_]](c: BuiltInCommand): Stream[F, Unit] = c match {
      case builtins.Quit => Stream.empty
      case builtins.Help => Stream.empty
    }

  }
}

abstract class Fs2JLine[F[_] : Sync, C] {

  val commands: NonEmptyList[Command[C]]

  def onCommand(c: C): Stream[F, Unit]

  // TODO This should be customizable at runtime.
  // For example, to display the current directory.
  def prompt: String

  private def delay[A](a: => A): Stream[F, A] =
    Stream.eval(Sync[F].delay(a))

  // TODO This should do something meaningful :)
  def onBuiltinCommand(c: BuiltInCommand): Stream[F, Unit] = c match {
    case builtins.Quit => Stream.empty
    case builtins.Help => Stream.empty
  }

  val terminal: Stream[F, Terminal] = delay(TerminalBuilder.builder()
    .system(true)
    .build())

  def reader(terminal: Terminal): Stream[F, LineReader] = delay(LineReaderBuilder.builder()
    .appName("testing")
    .terminal(terminal)
    .build())

  def readLine(reader: LineReader): Stream[F, NonEmptyList[String]] =
    Stream.eval(Sync[F].delay(blocking(reader.readLine(prompt))))
      .flatMap { line =>
        NonEmptyList.fromList(line.split(" ").toList) match {
          case None => Stream.empty
          case Some(nel) => Stream.emit(nel)
        }
      }

  // TODO This should return a Stream[F, Either[Command[Builtin.Cmd], Command[C]]] instead
  def findCommand(line: NonEmptyList[String]): Stream[F, Command[C]] = {
    commands.find(_.name == line.head) match {
      case None =>
        delay(println(s"Unknown command ${line.head}")) >> Stream.empty
      case Some(cmd) => Stream.emit(cmd)
    }
  }

  def parse(cmd: Command[C], args: List[String]): Stream[F, C] = {
    cmd.parse(args).fold(
      error => delay(println(error)) >> Stream.empty,
      c => Stream.emit(c)
    )
  }

  def loop(r: LineReader): Stream[F, Unit] = for {
    args <- readLine(r)
    cmd <- findCommand(args)
    c <- parse(cmd, args.tail)
    _ <- onCommand(c).handleErrorWith { e =>
      // TODO Rm side-effect
      println(s"Error while handling onCommand: $e")
      Stream.empty
    }
    // Looping instead of completing the stream, because if we do complete it then the .repeat combinator
    // doesn't catch it, and thus the overall stream stop.
    _ <- loop(r)
  } yield ()

  def runnableStream: Stream[F, ExitCode] = {
    for {
      t <- terminal
      r <- reader(t)
      _ <- loop(r).repeat
    } yield ExitCode.Success
  }

}
