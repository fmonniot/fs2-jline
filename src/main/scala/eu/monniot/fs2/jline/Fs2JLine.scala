package eu.monniot.fs2.jline

import cats.data.NonEmptyList
import cats.effect.Sync
import cats.implicits._
import com.monovore.decline.Command
import eu.monniot.fs2.jline.Fs2JLine.Prompt
import fs2.Stream
import fs2.StreamApp.ExitCode
import org.jline.reader.LineReader


object Fs2JLine {
  // TODO Offer a simpler constructor which doesn't manage the prompt at all
  def apply[F[_] : Sync, C, S](availableCommands: NonEmptyList[Command[C]])
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

abstract class Fs2JLine[F[_] : Sync, C, S] {

  // Arguments

  val commands: NonEmptyList[Command[C]]

  val initialState: F[S]

  val initialPrompt: F[Prompt]

  // Function

  def onCommand(c: C, s: S): F[(Prompt, S)]

  // Loop and Stream conversion

  import internal._

  def loop(r: LineReader, s: S, p: Prompt): Stream[F, Unit] = {
    for {
      args <- readLine(p, r)
      cmd <- findCommand(commands, args)
      c <- parse(cmd, args.tail)
      (np, ns) <- Stream.eval(onCommand(c, s)).handleErrorWith { e =>
        delay(println(s"Error while handling onCommand: $e")) >> Stream.empty
      }
      // Looping instead of completing the stream, because if we do complete it then the .repeat combinator
      // doesn't catch it, and thus the overall stream stop.
      _ <- loop(r, ns, np)
    } yield ()
  } ++ loop(r, s, p) // like t

  def stream: Stream[F, ExitCode] = {
    for {
      t <- terminal
      r <- reader(t)
      s <- Stream.eval(initialState)
      p <- Stream.eval(initialPrompt)
      _ <- loop(r, s, p)
    } yield ExitCode.Success
  }

}
