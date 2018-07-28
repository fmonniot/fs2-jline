package eu.monniot.fs2.jline.derive

import cats.implicits._
import cats.data.NonEmptyList
import com.monovore.decline.{Command, Opts}
import shapeless.{::, CNil, HList, HNil, LabelledGeneric, Lazy, Witness, tag}
import shapeless.labelled._
import shapeless.syntax.singleton._


/* First try based on a type class derivation thing.
 * Which isn't working as well as I had hoped.
 * Mainly because we aren't working with type class (duh !)
 */
object tryout1 {

  def invoke[A](implicit a: NonEmptyList[Command[A]]) = a

  //  implicit val lsCommand: Opts[Ls] = ???
  implicit val addCommand: Opts[Add.type] = ???
  implicit val rmCommand: Opts[Rm.type] = ???

  implicit val hNilCommand: Opts[HNil] = Opts.unit.map(_ => HNil)

  implicit def booleanOpts[K <: Symbol](implicit witness: Witness.Aux[K]): Opts[FieldType[K, Boolean]] = {
    // Here we need to define multiple function based on the type A
    // ie. one for boolean, one for list, and so on. For now we only have Boolean in our example

    val name = witness.value.name
    Opts.flag(name, name).orFalse.map(b => field[K](b))
  }

  implicit def hListOpts[K <: Symbol, H, T <: HList](implicit
                                                     hEncoder: Lazy[Opts[FieldType[K, H]]],
                                                     tEncoder: Opts[T]
                                                    ): Opts[FieldType[K, H] :: T] = {
    (hEncoder.value, tEncoder).mapN { case (h, t) =>
      h :: t
    }
  }

  implicit def genericOpts[A, H <: HList](implicit generic: LabelledGeneric.Aux[A, H],
                                          opts: Opts[H]): Opts[A] = {
    opts.map(h => generic.from(h))
  }

  LabelledGeneric[Ls]

  val long = 'long ->> true

  implicitly[Thing[Ls]]


  //  implicit def coProductCommandNel[K <: Symbol, H, T <: Coproduct]
  //  (implicit
  //   witness: Witness.Aux[K],
  //   hOpts: Lazy[Opts[H]],
  //   tCommands: NonEmptyList[Command[T]]
  //  ): NonEmptyList[Command[H :+: T]] = {
  //    val commandName = witness.value.name
  //    val commandHeader = commandName // TODO Use annotations later on
  //
  //    val command = Command(commandName, commandHeader)(hOpts.value)
  //    val c = command.map(Inl(_))
  //
  //    c :: tCommands.map(_.map(Inr(_)))
  //  }


  //    println(invoke[Cmd])
  //  println(implicitly[Opts[Ls]])
  //  println(implicitly[Opts[Add.type]])
  //  println(implicitly[Opts[Rm.type]])
  //  println(implicitly[NonEmptyList[Command[CNil]]])
  //  println(implicitly[NonEmptyList[Command[Rm.type :+: CNil]]])

}
