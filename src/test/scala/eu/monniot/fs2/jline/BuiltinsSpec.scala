package eu.monniot.fs2.jline

import cats.data.NonEmptyList
import com.monovore.decline.{Command, Opts}
import org.scalatest.{FlatSpec, Matchers}

class BuiltinsSpec extends FlatSpec with Matchers {

  import builtins._

  "#buildHelp" should "return a help text for a single command" in {
    val help = buildHelp(NonEmptyList.of(Command("name", "description")(Opts.never)))

    help shouldEqual
      """Commands:
        |    name
        |        description""".stripMargin
  }

  it should "not include commands arguments" in {
    val help = buildHelp(NonEmptyList.of(Command("name", "description")(Opts.argument[String]("argument"))))

    help shouldEqual
      """Commands:
        |    name
        |        description""".stripMargin
  }

  it should "return a help text for a list of commands" in {
    val help = buildHelp(NonEmptyList.of(
      Command("name", "description")(Opts.never),
      Command("subcommand", "For The Win !")(Opts.never)
    ))

    help shouldEqual
      """Commands:
        |    name
        |        description
        |    subcommand
        |        For The Win !""".stripMargin
  }

}
