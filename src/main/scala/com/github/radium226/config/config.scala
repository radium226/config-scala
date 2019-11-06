package com.github.radium226.config

import cats.effect._
import file.File
import arguments.Arguments
import cats.kernel.Monoid
import cats.implicits._
import com.typesafe.config.ConfigRenderOptions
import pureconfig._
import shapeless.HList


trait Config[F[_], A] {

  def parse(arguments: String*): F[A]

}

trait ConfigInstances {

  implicit def configForAny[F[_], A, PiecesOfA <: HList](implicit
    // Effects
    F: Sync[F],
    // Behavior
    behaviors: Behaviors,
    // PiecesOfA
    puzzleForA: Puzzle.Aux[F, A, PiecesOfA],
    PiecesOfA: Monoid[PiecesOfA],
    arguments: Arguments[F, PiecesOfA]
  ): Config[F, A] = new Config[F, A] {

    def parse(parameters: String*): F[A] = {
      val configObjectSource = Scope.values.map(behaviors.configObjectSource(_)).reduce(_.withFallback(_))
      for {
        piecesOfAFromArguments <- arguments.parse(configObjectSource, parameters: _*)
        piecesOfA               = PiecesOfA.empty.combine(piecesOfAFromArguments)
        a                      <- puzzleForA.assemble(piecesOfA)
      } yield a
    }
  }

}

object Config {

  def apply[F[_], A](implicit configForA: Config[F, A]): Config[F, A] = configForA

}
