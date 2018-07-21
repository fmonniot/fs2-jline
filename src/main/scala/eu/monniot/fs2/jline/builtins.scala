package eu.monniot.fs2.jline

import cats.Applicative
import cats.data.NonEmptyList
import cats.implicits._
import com.monovore.decline.{Command, Opts}

private[jline] object builtins {

  sealed trait BuiltInCommand

  case object Quit extends BuiltInCommand

  case object Help extends BuiltInCommand

  val commands: NonEmptyList[Command[BuiltInCommand]] = NonEmptyList.of(
    Command("quit", "Quit this shell")(Opts.unit.map(_ => Quit)),
    Command("q", "Quit this shell")(Opts.unit.map(_ => Quit)),
    Command("help", "Display the basic usage of this shell")(Opts.unit.map(_ => Quit)),
    Command("help", "Display the basic usage of this shell")(Opts.unit.map(_ => Quit)),
  )

  // TODO something useful here
  // And for that we will needs a real state
  // (containing the commands and a mutable Signal for help and quit respectively)
  def onBuiltinCommand[F[_]: Applicative](c: BuiltInCommand, s: Nothing): F[Unit] = c match {
    case builtins.Quit => Applicative[F].unit
    case builtins.Help => Applicative[F].unit
  }

}
