package eu.monniot.fs2.jline.derive

import com.monovore.decline.{Command, Opts}
import shapeless._
import shapeless.labelled.FieldType


/*
 * Another try based on shapeless' polymorphic functions.
 * The idea here is to generate a labelled HList from the concrete class and then
 * to map each labelled type to its own Opts.
 * We can then fold into an option of type, and from there map back to the concrete class.
 *
 * I have been able to make a hard-coded example, so that count as a win in my book :)
 * Next step is to generify this object into useable functions (remove the hard-coded dependency
 * on Ls) and see where this go.
 */
object polymorphic {

  // TODO Needs way more default cases than just that
  // Also, use annotation for more configuration options :)
  object mapper extends Poly1 {
    implicit def boolCase[K <: Symbol](implicit witness: Witness.Aux[K]): Case.Aux[FieldType[K, Boolean], Opts[Boolean]] =
      at(_ => Opts.flag(witness.value.name, "").orFalse)
  }

  // TODO Remove the hardcoded cases and use generic one
  object folder extends Poly2 {

    import cats.syntax.apply._

    // First case
    implicit val optsBoolEnd: Case.Aux[Opts[HNil], Opts[Boolean], Opts[Boolean :: HNil]] =
      at { (a, b) =>
        (a, b).mapN { case (c, d) => d :: c }
      }

    // Second case
    implicit val optsBoolCase: Case.Aux[Opts[Boolean :: HNil], Opts[Boolean], Opts[Boolean :: Boolean :: HNil]] =
      at { (a, b) =>
        (a, b).mapN { case (c, d) => d :: c }
      }
  }

  val ls = Ls(long = true, human = true)
  val optsHNil: Opts[HNil] = Opts.unit.map(_ => HNil)

  // TODO From now on, we should be able to abstract the Ls type

  val lg = LabelledGeneric[Ls]
  val lhlist = lg.to(ls)

  // TODO Here we have lost the field labels, and thus we will need a standard Generic[Ls] later on
  // See if we can keep the label here to not require both Generic kind.
  val gen2 = lhlist.map(mapper)

  val optsHList: Opts[Boolean :: Boolean :: HNil] = gen2.foldLeft(optsHNil)(folder)

  val gen = Generic[Ls]

  val lsOpts = optsHList.map(hList => gen.from(hList))
  val cmd = Command("ls", "ls")(lsOpts)

  cmd.parse(Seq())
  cmd.parse(Seq("--long"))
  cmd.parse(Seq("--human"))
  cmd.parse(Seq("--long", "--human"))
}
