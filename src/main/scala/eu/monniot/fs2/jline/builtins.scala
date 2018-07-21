package eu.monniot.fs2.jline

import cats.data.NonEmptyList
import cats.effect.{Concurrent, Sync}
import cats.implicits._
import com.monovore.decline.{Command, Opts}
import fs2.async.mutable.Signal

private[jline] object builtins {

  sealed trait BuiltInCommand

  case object Quit extends BuiltInCommand

  case object Help extends BuiltInCommand

  val commands: NonEmptyList[Command[BuiltInCommand]] = NonEmptyList.of(
    Command("quit", "Quit this shell")(Opts.unit.map(_ => Quit)),
    Command("q", "Quit this shell")(Opts.unit.map(_ => Quit)),
    Command("help", "Display the basic usage of this shell")(Opts.unit.map(_ => Quit)),
    Command("h", "Display the basic usage of this shell")(Opts.unit.map(_ => Quit)),
  )

  case class BuiltInCommandState[F[_]](requestShutdown: Signal[F, Boolean], commands: NonEmptyList[Command[_]])

  def initialBuiltInState[F[_]: Concurrent](commands: NonEmptyList[Command[_]]): F[BuiltInCommandState[F]] =
    for {
      signal <- fs2.async.signalOf[F, Boolean](false)
    } yield BuiltInCommandState(signal, commands concatNel builtins.commands)

  def onBuiltinCommand[F[_] : Sync](c: BuiltInCommand, s: BuiltInCommandState[F]): F[BuiltInCommandState[F]] = c match {
    case builtins.Quit =>
      Sync[F].delay(println("bye !")) *>
        s.requestShutdown.set(true) *>
        Sync[F].pure(s)

    case builtins.Help =>
      Sync[F].delay(println(buildHelp(s.commands))) *>
        Sync[F].pure(s)
  }

  def buildHelp(commands: NonEmptyList[Command[_]]): String = {
    val texts = commands.flatMap { command =>
      NonEmptyList.of(withIndent(4, command.name), withIndent(8, command.header))
    }
    ("Commands:" :: texts).toList.mkString("\n")
  }

  def withIndent(indent: Int, s: String): String = s.lines.map(" " * indent + _).mkString("\n")

}
