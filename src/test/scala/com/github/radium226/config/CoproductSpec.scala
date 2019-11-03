package com.github.radium226.config

import shapeless._
import shapeless.ops.coproduct.Folder

class CoproductSpec extends AbstractSpec {

  trait MakeFortyTwo[T] {

    type FortyTwo

    def apply(t: T): FortyTwo

  }

  object MakeFortyTwo {

    type Aux[A, FortyTwoOfA] = MakeFortyTwo[A] { type FortyTwo = FortyTwoOfA }

    implicit def forCNil: MakeFortyTwo.Aux[CNil, CNil] = new MakeFortyTwo[CNil] {

      override type FortyTwo = CNil

      def apply(cNil: CNil): FortyTwo = {
        cNil
      }

    }

    implicit def forCCons[H, FortyTwoOfH, T <: Coproduct, FortyTwoOfT <: Coproduct](implicit
      makeFortyTwoForH: MakeFortyTwo.Aux[H, FortyTwoOfH],
      makeFortyTwoForT: MakeFortyTwo.Aux[T, FortyTwoOfT]
    ): MakeFortyTwo.Aux[H :+: T, FortyTwoOfH :+: FortyTwoOfT] = new MakeFortyTwo[H :+: T] {

      override type FortyTwo = FortyTwoOfH :+: FortyTwoOfT

      def apply(hOrT: H :+: T): FortyTwo = {
        hOrT match {
          case Inl(h) =>
            Coproduct(makeFortyTwoForH(h)).asInstanceOf[FortyTwoOfH :+: FortyTwoOfT]
          case Inr(t) =>
            Coproduct(makeFortyTwoForT(t)).asInstanceOf[FortyTwoOfH :+: FortyTwoOfT]
        }
      }

    }

    implicit def forCoproduct[A, ReprOfA <: Coproduct, FortyTwoOfReprOfA <: Coproduct](implicit
      generic: Generic.Aux[A, ReprOfA],
      makeFortyTwoForReprOfA: MakeFortyTwo.Aux[ReprOfA, FortyTwoOfReprOfA]
    ): MakeFortyTwo.Aux[A, FortyTwoOfReprOfA] = new MakeFortyTwo[A] {

      override type FortyTwo = FortyTwoOfReprOfA

      def apply(a: A): FortyTwo = {
        makeFortyTwoForReprOfA(generic.to(a))
      }

    }

    implicit def makeFortyTwoForInt: MakeFortyTwo.Aux[Int, String] = new MakeFortyTwo[Int] {

      override type FortyTwo = String

      override def apply(i: Int): FortyTwo = {
        "42"
      }

    }

    implicit def makeFortyTwoForString: MakeFortyTwo.Aux[String, String] = new MakeFortyTwo[String] {

      override type FortyTwo = String

      override def apply(s: String): FortyTwo = {
        "Forty Two"
      }

    }

    def apply[A](implicit makeFortyTwoForA: MakeFortyTwo[A]): MakeFortyTwo[A] = makeFortyTwoForA

  }

  type StringOrInt = String :+: Int :+: CNil

  val makeFortyTwoForStringOrInt = MakeFortyTwo[StringOrInt]

  it should "be able to have a coproduct" in {
    println(makeFortyTwoForStringOrInt.apply(Coproduct[StringOrInt]("Hello")))
    println(makeFortyTwoForStringOrInt.apply(Coproduct[StringOrInt](1)))
  }

}
