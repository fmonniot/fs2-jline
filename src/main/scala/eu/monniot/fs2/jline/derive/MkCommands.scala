package eu.monniot.fs2.jline.derive

import com.monovore.decline.Command
import shapeless.{:+:, CNil, Coproduct, LabelledGeneric}


trait MkCommands[T] {
  protected def commands: List[Any]
}

object MkCommands {

  // We don't expose the unsafe MkCommands' types field
  def apply[T](implicit mkCommands: MkCommands[T]): List[Command[T]] =
    mkCommands.commands.asInstanceOf[List[Command[T]]]

  implicit def caseCNil: MkCommands[CNil] = new MkCommands[CNil] {
    def commands: List[Any] = Nil
  }

  implicit def caseCCons[H, T <: Coproduct](implicit tail: MkCommands[T],
                                            mk: MkCommand[H]): MkCommands[H :+: T] =
    new MkCommands[H :+: T] {
      def commands: List[Any] = mk.cmd :: tail.commands
    }

  implicit def genericCoproduct[A, C <: Coproduct](implicit gen: LabelledGeneric.Aux[A, C],
                                                   iterate: MkCommands[C]
                                                  ): MkCommands[A] =
    new MkCommands[A] {
      override def commands: List[Any] = iterate.commands
    }

}
