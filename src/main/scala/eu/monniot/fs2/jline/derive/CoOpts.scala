package eu.monniot.fs2.jline.derive

import cats.Alternative
import com.monovore.decline.Opts

// Invariant ops
// Just a trick to make shapeless derivation work (afaik)
sealed trait CoOpts[T] {
  def opts: Opts[T]
}

object CoOpts {
  def instance[T](opts0: Opts[T]): CoOpts[T] =
    new CoOpts[T] {
      override def opts = opts0
    }

  // Type class instance

  implicit val alternative: Alternative[CoOpts] =
    new Alternative[CoOpts] {
      def pure[A](x: A): CoOpts[A] =
        instance(Alternative[Opts].pure(x))

      def ap[A, B](ff: CoOpts[A => B])(fa: CoOpts[A]): CoOpts[B] =
        instance(Alternative[Opts].ap(ff.opts)(fa.opts))

      def empty[A]: CoOpts[A] =
        instance(Alternative[Opts].empty)

      def combineK[A](x: CoOpts[A], y: CoOpts[A]): CoOpts[A] =
        instance(Alternative[Opts].combineK(x.opts, y.opts))
    }

  // shortcut for common Opts functions
  val never: CoOpts[Nothing] = instance(Opts.never)

  def unit: CoOpts[Unit] = instance(Opts.unit)

}
