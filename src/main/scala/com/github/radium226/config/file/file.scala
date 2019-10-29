package com.github.radium226.config.file

import java.nio.file.Path

import pureconfig._
import cats.effect._
import cats.implicits._
import com.github.radium226.config.{Behaviors, Scope}


trait File[F[_], A] {

  def parse(content: String): F[A]

}

object File {

  def apply[F[_], A](implicit fileForA: File[F, A]): File[F, A] = fileForA

}

trait FileInstances {

  implicit def fileForAny[F[_], A](implicit
    F: Sync[F],
    derivationForConfigReaderForA: Derivation[ConfigReader[A]],
    behavior: Behaviors
  ): File[F, A] = new File[F, A] {

    override def parse(content: String): F[A] = {
      ConfigSource.string(content).load[A].fold(
        { _ =>
          F.raiseError(new Exception("Unable to parse config"))
        },
        F.pure(_)
      )
    }
  }

}
