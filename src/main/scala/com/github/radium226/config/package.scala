package com.github.radium226

package object config extends AllInstances {

  sealed trait Error

  type Piece[A] = Option[A]

  object Piece {

    def apply[A](a: A): Piece[A] = Some(a)

    def missing: Piece[Nothing] = None

  }

  def debug(message: String): Unit = {
    println(s" --> ${message}")
  }

}
