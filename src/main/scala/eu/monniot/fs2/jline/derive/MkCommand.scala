package eu.monniot.fs2.jline.derive

import com.monovore.decline.{Command, Opts}
import shapeless.Witness
import shapeless.labelled.{FieldType, field}

trait MkCommand[A] {
  def cmd: Command[A]
}

object MkCommand {

  implicit def cocommand[K <: Symbol, A](implicit witness: Witness.Aux[K],
                                         mkCoOpts: MkCoOpts[A]): MkCommand[FieldType[K,A]] = {
    // We will need annotations for other command attributes
    val name = witness.value.name
    val opts: Opts[FieldType[K, A]] = mkCoOpts.coOpts.opts.map(a => field[K](a))

    new MkCommand[FieldType[K, A]] {
      def cmd = Command(name, name)(opts)
    }
  }
}
