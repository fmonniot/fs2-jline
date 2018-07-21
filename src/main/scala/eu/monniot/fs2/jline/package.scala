package eu.monniot.fs2

import cats.implicits._
import cats.data.NonEmptyList
import cats.effect.Concurrent
import com.monovore.decline.Command
import eu.monniot.fs2.jline.Fs2JLine.Prompt
import fs2.Stream
import fs2.StreamApp.ExitCode

package object jline {

  def apply[F[_] : Concurrent, C, S](availableCommands: NonEmptyList[Command[C]])
                                    (state: F[S], prompt: F[Prompt])
                                    (f: (C, S) => F[(Prompt, S)]): Stream[F, ExitCode] =
    new Fs2JLine[F, C, S] {
      override val commands = availableCommands

      override val initialState = state

      override def onCommand(c: C, s: S): F[(Prompt, S)] = f(c, s)

      override val initialPrompt = prompt
    }.stream

  // A simpler version which doesn't deal with the prompt
  def simple[F[_]: Concurrent, C, S](availableCommands: NonEmptyList[Command[C]], state: F[S])
                                    (f: (C, S) => F[S]): Stream[F, ExitCode] = {
    new Fs2JLine[F, C, S] {
      val prompt = ">"
      override val commands = availableCommands
      override val initialState = state
      override val initialPrompt = Concurrent[F].pure(prompt)

      override def onCommand(c: C, s: S): F[(Prompt, S)] =
        f(c, s).map(ns => (prompt, ns))
    }.stream
  }
}
