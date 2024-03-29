package com.github.radium226.config

import cats.kernel.Monoid
import cats.implicits._

import shapeless._
import shapeless.labelled._

trait MonoidInstances {

  implicit def monoidForHNil: Monoid[HNil] = new Monoid[HNil] {

    override def empty: HNil = HNil

    override def combine(x: HNil, y: HNil): HNil = HNil

  }

  implicit def monoidForHCons[K <: Symbol, H, T <: HList](implicit
    T: Monoid[T]
  ): Monoid[FieldType[K, Piece[H]] :: T] = new Monoid[FieldType[K, Piece[H]] :: T] {

    override def empty: FieldType[K, Piece[H]] :: T = {
      field[K](Piece.missing) :: T.empty
    }

    override def combine(x: FieldType[K, Piece[H]] :: T, y: FieldType[K, Piece[H]] :: T): FieldType[K, Piece[H]] :: T = {
      field[K](x.head.asInstanceOf[Piece[H]].orElse(y.head.asInstanceOf[Piece[H]])) :: T.combine(x.tail, y.tail)
    }

  }

}
