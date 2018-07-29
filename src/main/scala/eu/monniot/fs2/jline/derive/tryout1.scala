package eu.monniot.fs2.jline.derive

import cats.implicits._
import com.monovore.decline.Opts
import eu.monniot.fs2.jline.derive.Cmd.Ls
import shapeless.Witness
import shapeless.labelled.{FieldType, field}


/* First try based on a type class derivation thing.
 * Which isn't working as well as I had hoped.
 * Mainly because we aren't working with type class (duh !)
 */
object tryout1 {

  implicit def booleanCoOpts[K <: Symbol](implicit witness: Witness.Aux[K]): CoOpts[FieldType[K, Boolean]] = {
    // Here we need to define multiple function based on the type A
    // ie. one for boolean, one for list, and so on. For now we only have Boolean in our example
    // We will also have to introduce annotations support for the different options (eg. short, visibility, …)

    val name = witness.value.name
    val opts = Opts.flag(name, name).orFalse.map(b => field[K](b))

    CoOpts.instance(opts)
  }


  case class Thing()

  val mk1 = MkCoOpts[Thing]
  val mk2 = MkCoOpts[Ls]

}
