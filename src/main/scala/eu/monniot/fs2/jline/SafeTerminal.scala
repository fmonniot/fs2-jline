package eu.monniot.fs2.jline

import cats.data.NonEmptyList
import cats.effect.{IO, Sync}
import cats.implicits._
import com.monovore.decline.{Command, Opts}
import fs2._


object SafeTerminal {

  // The command ADT

  trait Cmd

  case class Ls(long: Boolean, symlink: Boolean, human: Boolean) extends Cmd

  case object Add extends Cmd

  case object Rm extends Cmd

  // command parsers
  // TODO Find a way to derive that from the ADT

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

  val commands: NonEmptyList[Command[Cmd]] = NonEmptyList.of(lsCmd, addCmd, rmCmd)

  // Command implementation

  def app[F[_] : Sync](c: Cmd): Stream[F, Unit] = c match {
    case Ls(long, symlink, human) =>
      Stream.eval(Sync[F].delay {
        println(s"listing stuff with long = $long, symlink = $symlink and human = $human")
      })

    case Add =>
      Stream.eval(Sync[F].delay(println("Adding stuff")))

    case Rm =>
      Stream.eval(Sync[F].delay(println("")))
  }

}
