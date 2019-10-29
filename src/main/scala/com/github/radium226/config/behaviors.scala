package com.github.radium226.config

import java.nio.file.Path

import cats.effect.Sync
import com.google.common.base.CaseFormat

import scala.io.Source

trait Behaviors {

  def inferOptionName(key: Symbol): String

  def inferSubcommandName(runtimeClass: Class[_]): String

  def readContent[F[_]](application: String, module: String, scope: Scope)(implicit F: Sync[F]): F[String]

}

class DefaultBehaviors extends Behaviors {

  def inferOptionName(key: Symbol): String = {
    CaseFormat.LOWER_CAMEL.to(CaseFormat.LOWER_HYPHEN, key.name)
  }

  def inferSubcommandName(runtimeClass: Class[_]): String = {
    CaseFormat.UPPER_CAMEL.to(CaseFormat.LOWER_HYPHEN, runtimeClass.getSimpleName.toLowerCase)
  }

  def readContent[F[_]](application: String, module: String, scope: Scope)(implicit F: Sync[F]): F[String] = ???

}

trait BehaviorsInstances {

  implicit def behaviorsForAny: Behaviors = new DefaultBehaviors

}
