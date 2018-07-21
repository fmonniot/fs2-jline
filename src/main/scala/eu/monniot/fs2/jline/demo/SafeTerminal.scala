package eu.monniot.fs2.jline.demo

import cats.data.NonEmptyList
import cats.effect.Sync
import cats.implicits._
import com.monovore.decline.{Command, Opts}
import eu.monniot.fs2.jline.Fs2JLine.Prompt


object SafeTerminal {

  // The command ADT

  trait Cmd

  case class Ls(long: Boolean, human: Boolean) extends Cmd

  case object Add extends Cmd

  case object Rm extends Cmd

  // command parsers
  // TODO Find a way to derive that from the ADT

  val lsCmd = Command[Ls]("ls", "List some files") {
    val long = Opts.flag("long", "Long format", "l").orFalse
    val human = Opts.flag("human", "Human readable sizes", "h").orFalse

    (long, human).mapN { (l, h) =>
      Ls(l, h)
    }
  }
  val addCmd = Command[Add.type]("add", "Add something")(Opts(Add))
  val rmCmd = Command[Rm.type]("rm", "Remove something")(Opts(Rm))

  val commands: NonEmptyList[Command[Cmd]] = NonEmptyList.of(lsCmd, addCmd, rmCmd)

  // Command implementation

  type State = Int

  def apply[F[_]](c: Cmd, state: State)(implicit F: Sync[F]): F[(Prompt, State)] = c match {
    case Ls(long, human) =>

      val txt = if(long) "Number of successful command ran (minus this one): " else ""
      val n = if(human) numberToEnglish(state) else state.toString

      F.delay {
        println(s"$txt$n time${if(state > 0) "s" else ""}")
      } >> F.pure((">", state + 1))

    case Add =>
      F.delay {
        println("Adding stuff")
      } >> F.pure((">", state + 1))

    case Rm =>
      F.delay {
        println("Removing stuff")
      } >> F.pure((">", state + 1))
  }

  def numberToEnglish(num: Int): String = {
    if (num < 0) s"negative ${numberToEnglish(-num)}"
    else if (num >= 1000000000) s"${numberToEnglish(num / 1000000000)} billion ${numberToEnglish(num % 1000000000)}"
    else if (num >= 1000000) s"${numberToEnglish(num / 1000000)} million ${numberToEnglish(num % 1000000)}"
    else if (num >= 1000) s"${numberToEnglish(num / 1000)} thousand ${numberToEnglish(num % 1000)}"
    else if (num >= 100) s"${numberToEnglish(num / 100)} hundred ${numberToEnglish(num % 100)}"
    else if (num >= 20) num/10 match {
      case 2 => s"twenty ${numberToEnglish(num % 10)}"
      case 3 => s"thirty ${numberToEnglish(num % 10)}"
      case 5 => s"fifty ${numberToEnglish(num % 10)}"
      case n@_ => s"${numberToEnglish(n).stripSuffix("t")}ty ${numberToEnglish(num % 10)}"
    }
    else num match {
      case 0 => ""
      case 1 => "one"
      case 2 => "two"
      case 3 => "three"
      case 4 => "four"
      case 5 => "five"
      case 6 => "six"
      case 7 => "seven"
      case 8 => "eight"
      case 9 => "nine"
      case 10 => "ten"
      case 11 => "eleven"
      case 12 => "twelve"
      case 13 => "thirteen"
      case 15 =>"fifteen";
      case n@_ => s"${numberToEnglish(n-10).stripSuffix("t")}teen"
    }
  }
}
