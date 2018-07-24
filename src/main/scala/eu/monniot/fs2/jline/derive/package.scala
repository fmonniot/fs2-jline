package eu.monniot.fs2.jline

import cats.implicits._

package object derive {

  // Given a root product, create a Command[_]
  // Given a root coproduct, create a NonEmptyList[Command[_]]
  // For non-root coproduct, create subcommands

  // We should follow case-app convention, as it seems to be a well known library.


  sealed trait Cmd

  case class Ls(long: Boolean, human: Boolean) extends Cmd

  case object Add extends Cmd

  case object Rm extends Cmd

}
