package eu.monniot.fs2.jline

import cats.data.NonEmptyList
import cats.implicits._
import com.monovore.decline.Opts
import eu.monniot.fs2.jline.Commands.MkRootCommands
import shapeless.{Coproduct, HList, LabelledGeneric, ops}

import scala.annotation.implicitNotFound

package object derive {

  // Given a root product, create a Command[_]
  // Given a root coproduct, create a NonEmptyList[Command[_]]
  // For non-root coproduct, create subcommands

  // We should follow case-app convention, as it seems to be a well known library.


  sealed trait Cmd

  case class Ls(long: Boolean, human: Boolean) extends Cmd

  case object Add extends Cmd

  case object Rm extends Cmd


  def command[T : MkCoOpts]: Opts[T] = {
    val make = MkCoOpts[T]

    make.coOpts.opts
  }

  def rootCommands[T: MkRootCommands]: NonEmptyList[Opts[T]] = MkRootCommands[T].commands

  val rc = rootCommands[Cmd]

}

object Commands {

  @implicitNotFound("MkRootCommands can only be deduced for sealed trait.")
  trait MkRootCommands[A] {
    def commands: NonEmptyList[Opts[A]]
  }

  object MkRootCommands {
    def apply[T](implicit mk: MkRootCommands[T]): MkRootCommands[T] = mk

    implicit def coproduct[A, C <: Coproduct, K <: HList](implicit
                                                          gen: LabelledGeneric.Aux[A, C],
                                                          keys: ops.union.Keys.Aux[C, K],
                                                          toSet: ops.hlist.ToTraversable.Aux[K, Set, Symbol]
                                                         ): MkRootCommands[A] = {

      val set = toSet(keys())
      val names = set.map(_.name)
      println(names)

      val c: NonEmptyList[Opts[A]] = ???

      new MkRootCommands[A] {
        def commands = c
      }
    }

  }

}
