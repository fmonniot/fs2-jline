package eu.monniot.fs2.jline.derive

import cats.implicits._
import cats.data.NonEmptyList
import com.monovore.decline.{Command, Opts}
import shapeless.{::, CNil, HList, HNil, LabelledGeneric, Lazy, Witness}
import shapeless.labelled.FieldType


/* First try based on a type class derivation thing.
 * Which isn't working as well as I had hoped.
 * Mainly because we aren't working with type class (duh !)
 */
object tryout1 {

  def invoke[A](implicit a: NonEmptyList[Command[A]]) = a

  //  implicit val lsCommand: Opts[Ls] = ???
  implicit val addCommand: Opts[Add.type] = ???
  implicit val rmCommand: Opts[Rm.type] = ???

  implicit val cNilCommand: NonEmptyList[Command[CNil]] = ???

  implicit val hNilCommand: Opts[HNil] = Opts.never

  //  implicit def hListEncoder[H, T <: HList](implicit hEncoder: Opts[H],
  //                                           tEncoder: Opts[T]
  //                                          ): Opts[H :: T] = {
  //    (hEncoder, tEncoder).mapN { case (h, t) => h :: t }
  //  }

  implicit def hListObjectEncoder[K <: Symbol, H, T <: HList](implicit
                                                              witness: Witness.Aux[K],
                                                              hEncoder: Lazy[Opts[FieldType[K, H]]],
                                                              tEncoder: Opts[T]
                                                             ): Opts[FieldType[K, H] :: T] = {
    val fieldName: String = witness.value.name

    (hEncoder.value, tEncoder).mapN { case (h, t) =>
      h :: t
    }
  }

  trait Thing[A] {

  }

  implicit def genericObjectEncoder[A, H <: HList](implicit generic: LabelledGeneric.Aux[A, H]): Thing[A] = {

    println(generic)

    new Thing[A] {}
  }

  LabelledGeneric[Ls]

  implicit val optsBoolean: Opts[Boolean] = ???

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
