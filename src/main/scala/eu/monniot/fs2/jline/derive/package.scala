package eu.monniot.fs2.jline

import cats.implicits._
import com.monovore.decline.Opts
import shapeless.LabelledGeneric

package object derive {

  // Given a root product, create a Command[_]
  // Given a root coproduct, create a NonEmptyList[Command[_]]
  // For non-root coproduct, create subcommands

  // We should follow case-app convention, as it seems to be a well known library.


  sealed trait Cmd

  object Cmd {

    case class Ls(long: Boolean, human: Boolean) extends Cmd

    case object Add extends Cmd

    case object Rm extends Cmd

  }


  def command[T: MkCoOpts]: Opts[T] = {
    val make = MkCoOpts[T]

    make.coOpts.opts
  }

  //  def rootCommands[T: MkRootCommands]: NonEmptyList[Opts[T]] = MkRootCommands[T].commands

  //  val rc = rootCommands[Cmd]

  val gen = LabelledGeneric[Cmd]
  val test = MkCommands[Cmd].types

}
