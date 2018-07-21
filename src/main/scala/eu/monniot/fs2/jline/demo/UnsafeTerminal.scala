package eu.monniot.fs2.jline.demo

import java.util.concurrent.atomic.AtomicBoolean

import org.jline.builtins.Completers.{FileNameCompleter, TreeCompleter}
import org.jline.reader._
import org.jline.reader.impl.completer.{AggregateCompleter, ArgumentCompleter, StringsCompleter}
import org.jline.terminal.TerminalBuilder

object UnsafeTerminal {

  val ls = new ArgumentCompleter(
    new StringsCompleter("ls"),
    new StringsCompleter("-l", "-L", "-h"),
    new FileNameCompleter()
  )

  val completer = new TreeCompleter(
    TreeCompleter.node("add"),
    TreeCompleter.node("rm")
  )

  def apply(): Unit = {
    val terminal = TerminalBuilder.builder()
      .system(true)
      .build()

    val reader = LineReaderBuilder.builder()
      .appName("testing")
      .terminal(terminal)
      .completer(new AggregateCompleter(ls, completer))
      .build()

    val running = new AtomicBoolean(true)
    while(running.get()) {
      reader.readLine(">") match {
        case "quit" =>
          println("bye")
          running.set(false)
        case unknown =>
          // Got the command here
          println(unknown)
      }
    }
  }
}
