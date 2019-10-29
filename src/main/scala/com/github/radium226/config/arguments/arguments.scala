package com.github.radium226.config.arguments

import cats._
import cats.effect.Sync
import cats.implicits._

import scala.reflect.ClassTag

import com.github.radium226.config.debug


trait Arguments[F[_], A] {

  def parse(parameters: String*): F[A]

}

object Arguments {

  def apply[F[_], A](implicit argumentsForA: Arguments[F, A]): Arguments[F, A] = argumentsForA

}

trait ArgumentsInstances {

  implicit def argumentsForAny[F[_], A](implicit
    F: Sync[F],
    makeCommandForA: MakeCommand[A],
    classTagForA: ClassTag[A]
  ): Arguments[F, A] = new Arguments[F, A] {

    def parse(parameters: String*): F[A] = {
      debug(s"argumentsForAny[F[_], ${classTagForA.runtimeClass.getSimpleName}")
      makeCommandForA()
        .parse(parameters)
        .fold(
          { help => F.raiseError(new Exception(help.usage.mkString(","))) }, F.pure(_))
    }
  }

}
