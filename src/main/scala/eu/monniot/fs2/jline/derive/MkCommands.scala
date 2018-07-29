package eu.monniot.fs2.jline.derive

import com.monovore.decline.{Command, Opts}
import shapeless.{:+:, CNil, Coproduct, LabelledGeneric, Witness}
import shapeless.labelled.{FieldType, field}

trait BuildRootCommand[A] {
  def c: Command[A]
}

object BuildRootCommand {

  implicit def cocommand[K <: Symbol, A](implicit witness: Witness.Aux[K]): BuildRootCommand[FieldType[K,A]] = {

    val name = witness.value.name
    // This one should be implicit via MkCoOpts
    val opts: Opts[A] = Opts.never.map(identity)
    val opts2: Opts[FieldType[K, A]] = opts.map(a => field[K](a))

    new BuildRootCommand[FieldType[K, A]] {
      def c = Command(name, name)(opts2)
    }
  }
}

trait MkCommands[T] { def types: List[Any] }

object MkCommands {
  def apply[T](implicit mkCommands: MkCommands[T]) = mkCommands

  implicit def caseCNil: MkCommands[CNil] = new MkCommands[CNil] {
    def types: List[Any] = Nil
  }

  implicit def caseCCons[H, T <: Coproduct]
  (implicit
   rec: MkCommands[T]
   , br: BuildRootCommand[H]
  ): MkCommands[H :+: T] =
    new MkCommands[H :+: T] {
      def types: List[Any] = br.c :: rec.types
    }

  implicit def genericCoproduct[A, C <: Coproduct](implicit gen: LabelledGeneric.Aux[A, C]
                                                    , iterate: MkCommands[C]
                                                   ): MkCommands[A] =
    new MkCommands[A] {
      override def types: List[Any] = iterate.types
    }

}
