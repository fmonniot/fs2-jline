package eu.monniot.fs2.jline

import cats.data.NonEmptyList
import cats.effect.Concurrent
import cats.implicits._
import com.monovore.decline.Command
import eu.monniot.fs2.jline.Fs2JLine.Prompt
import eu.monniot.fs2.jline.builtins.BuiltInCommandState
import fs2.Stream
import fs2.StreamApp.ExitCode
import org.jline.reader.LineReader


// TODO Use package object instead
object Fs2JLine {
  // TODO Offer a simpler constructor which doesn't manage the prompt at all
  def apply[F[_] : Concurrent, C, S](availableCommands: NonEmptyList[Command[C]])
                                    (state: F[S], prompt: F[Prompt])
                                    (f: (C, S) => F[(Prompt, S)]): Stream[F, ExitCode] =
    new Fs2JLine[F, C, S] {
      override val commands = availableCommands

      override val initialState = state

      override def onCommand(c: C, s: S): F[(Prompt, S)] = f(c, s)

      override val initialPrompt = prompt
    }.stream

  // Might or might not make sense
  type Prompt = String
}

abstract class Fs2JLine[F[_] : Concurrent, C, S] {

  // Arguments

  val commands: NonEmptyList[Command[C]]

  val initialState: F[S]

  val initialPrompt: F[Prompt]

  // Function

  def onCommand(c: C, s: S): F[(Prompt, S)]

  // Loop and Stream conversion

  import internal._

  def loop(r: LineReader, s: S, p: Prompt, bics: BuiltInCommandState[F]): Stream[F, Unit] = {
    for {
      // TODO Intercept JLine exception (eg. EndOfFileException or UserInterruptException)
      args <- readLine(p, r)

      // Find and execute builtins or custom command
      cmd <- findCommand(commands, args)
      res <- cmd.fold(
        execCommand(builtins.onBuiltinCommand(_, bics).map(_.asLeft), args.tail),
        execCommand(onCommand(_, s).map(_.asRight), args.tail)
      )

      // New state
      (ns, np, nbics) = res.fold(
        b => (s, p, b),
        n => (n._2, n._1, bics)
      )
      // Looping instead of completing the stream, because if we do complete it then the .repeat combinator
      // doesn't catch it, and thus the overall stream stop.
      _ <- loop(r, ns, np, nbics)
    } yield ()
  } ++ loop(r, s, p, bics) // like the fs2 repeat combinator, but with preserved state

  def stream: Stream[F, ExitCode] = {
    for {
      t <- terminal
      r <- reader(t)
      s <- Stream.eval(initialState)
      p <- Stream.eval(initialPrompt)
      bics <- Stream.eval(builtins.initialBuiltInState(commands))
      _ <- loop(r, s, p, bics).interruptWhen(bics.requestShutdown)
    } yield ExitCode.Success
  }

}
