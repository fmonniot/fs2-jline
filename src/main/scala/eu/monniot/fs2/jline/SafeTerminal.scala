package eu.monniot.fs2.jline

import cats.data.NonEmptyList
import cats.effect.{IO, Sync}
import cats.implicits._
import com.monovore.decline.{Command, Opts}
import fs2.StreamApp.ExitCode
import fs2._
import org.jline.reader.{LineReader, LineReaderBuilder}
import org.jline.terminal.{Terminal, TerminalBuilder}

import scala.concurrent.blocking

object SafeTerminal {

  // sample stuff

  trait Cmd

  case class Ls(long: Boolean, symlink: Boolean, human: Boolean) extends Cmd

  case object Add extends Cmd

  case object Rm extends Cmd

  val lsCmd = Command[Ls]("ls", "List some files") {
    val long = Opts.flag("long", "Long format", "l").orFalse
    val symlink = Opts.flag("symlink", "Follow symlink", "L").orFalse
    val human = Opts.flag("human", "Human readable sizes", "h").orFalse

    (long, symlink, human).mapN { (l, s, h) =>
      Ls(l, s, h)
    }
  }
  val addCmd = Command[Add.type]("add", "Add something")(Opts(Add))
  val rmCmd = Command[Rm.type]("rm", "Remove something")(Opts(Rm))

  // TODO Find a way to derive that from the ADT
  val commands: NonEmptyList[Command[Cmd]] = NonEmptyList.of(lsCmd, addCmd, rmCmd)

  def app[F[_] : Sync]: Fs2JLine[F, Cmd] = Fs2JLine(commands) {
    case Ls(long, symlink, human) =>
      Stream.eval(Sync[F].delay {
        println(s"listing stuff with long = $long, symlink = $symlink and human = $human")
      })

    case Add =>
      Stream.eval(Sync[F].delay(println("Adding stuff")))

    case Rm =>
      Stream.eval(Sync[F].delay(println("")))
  }

  // The lib should offer a JLineApp abstract class with this method already defined in it.
  def main(args: Array[String]): Unit = {
    import scala.concurrent.ExecutionContext.Implicits.global
    new StreamApp[IO]() {
      override def stream(args: List[String], requestShutdown: IO[Unit]) =
        app[IO].runnableStream
    }.main(args)
  }

  // lib stuff

  trait Commandable[A] {
    def commandsOpts: NonEmptyList[Command[A]]
  }

  object Fs2JLine {
    def apply[F[_] : Sync, C](availableCommands: NonEmptyList[Command[C]], defaultPrompt: String = ">")
                             (f: C => Stream[F, Unit]): Fs2JLine[F, C] =
      new Fs2JLine[F, C] {
        override val commands = availableCommands

        override def onCommand(c: C) = f(c)

        override def prompt = defaultPrompt
      }
  }

  abstract class Fs2JLine[F[_] : Sync, C] {

    val commands: NonEmptyList[Command[C]]

    def onCommand(c: C): Stream[F, Unit]

    // TODO This should be customizable at runtime.
    // For example, to display the current directory.
    def prompt: String

    //

    private def delay[A](a: => A): Stream[F, A] =
      Stream.eval(Sync[F].delay(a))

    def runnableStream: Stream[F, ExitCode] = {
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

      for {
        t <- terminal
        r <- reader(t)
        _ <- loop(r).repeat

      } yield ExitCode.Success
    }

  }

}
