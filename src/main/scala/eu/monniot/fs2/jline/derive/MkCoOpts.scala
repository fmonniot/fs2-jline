package eu.monniot.fs2.jline.derive

import cats.implicits._
import cats.Alternative
import shapeless._

trait MkCoOpts[T] {

  def coOpts: CoOpts[T]

}

object MkCoOpts {

  def apply[T](implicit mkOpts: MkCoOpts[T]): MkCoOpts[T] = mkOpts

  def instance[T](opts0: CoOpts[T]): MkCoOpts[T] =
    new MkCoOpts[T] {
      def coOpts = opts0
    }

  implicit def genericProduct[P, L <: HList](implicit gen: LabelledGeneric.Aux[P, L],
                                             mk: Lazy[MkHListOpts[L]]): MkCoOpts[P] =
    instance(mk.value.coOpts.map(gen.from))

//  implicit def genericProduct[P, L <: HList]
  //  (implicit
  //   gen: Generic.Aux[P, L],
  //   cogen: Lazy[MkHListCogen[L]]
  //  ): MkCogen[P] = instance(Cogen { (seed: Seed, p: P) =>
  //    cogen.value.cogen.perturb(seed, gen.to(p))
  //  })
  //
  //  implicit def genericCoproduct[S, C <: Coproduct]
  //  (implicit
  //   gen: Generic.Aux[S, C],
  //   cogen: Lazy[MkCoproductCogen[C]]
  //  ): MkCogen[S] = instance(Cogen { (seed: Seed, s: S) =>
  //    cogen.value.cogen.perturb(seed, gen.to(s))
  //  })

}

trait MkHListOpts[L <: HList] {
  def coOpts: CoOpts[L]
}

object MkHListOpts {
  def apply[L <: HList](implicit mkHListOpts: MkHListOpts[L]): MkHListOpts[L] = mkHListOpts

  def instance[L <: HList](coOpts0: => CoOpts[L]): MkHListOpts[L] =
    new MkHListOpts[L] {
      override def coOpts = coOpts0
    }


  implicit lazy val hNil: MkHListOpts[HNil] = {
    instance(Alternative[CoOpts].map(CoOpts.unit)(_ => HNil))
  }

  implicit def hCons[H, T <: HList](implicit headCoOpts: Strict[CoOpts[H]],
                                    tailCoOpts: MkHListOpts[T]): MkHListOpts[H :: T] =
    instance {
        (headCoOpts.value, tailCoOpts.coOpts).mapN { case (h, t) => h :: t }
    }

}

trait MkCoproductOpts[C <: Coproduct] {
  def coOpts: CoOpts[C]
}

object MkCoproductOpts {
  def apply[C <: Coproduct](implicit mk: MkCoproductOpts[C]): MkCoproductOpts[C] = mk

  def instance[C <: Coproduct](coOpts0: => CoOpts[C]): MkCoproductOpts[C] =
    new MkCoproductOpts[C] {
      override def coOpts = coOpts0
    }

  implicit lazy val cNil: MkCoproductOpts[CNil] =
    instance(Alternative[CoOpts].map(CoOpts.never)(identity))

  implicit def cCons[H, T <: Coproduct](implicit h: Strict[CoOpts[H]],
                                        t: MkCoproductOpts[T]): MkCoproductOpts[H :+: T] = {

    val cooptsH = h.value
    val cooptsT  = t.coOpts


//    def f: H :+: T => Mk



    ???
  }
}