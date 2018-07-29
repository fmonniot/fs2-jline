package eu.monniot.fs2.jline

import cats.data.NonEmptyList
import cats.implicits._
import com.monovore.decline.{Command, Opts}

package object derive {

  // [x] Given a root product, create a Command[_]
  // [x] Given a root coproduct, create a NonEmptyList[Command[_]]
  // [ ] For non-root coproduct, create sub commands

  // [ ] We should follow case-app convention, as it seems to be a well known library.
  // [ ] And this include the usage of annotations

  def command[T: MkCoOpts]: Opts[T] =
    MkCoOpts[T].coOpts.opts

  def commands[T: MkCommands]: Option[NonEmptyList[Command[T]]] =
    NonEmptyList.fromList(MkCommands[T])

}
