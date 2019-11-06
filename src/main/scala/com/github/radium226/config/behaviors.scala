package com.github.radium226.config

import java.nio.file.Path

import cats.effect.Sync
import com.google.common.base.CaseFormat
import pureconfig.{ConfigObjectSource, ConfigSource}

import scala.io.Source

trait Behaviors {

  def inferOptionName(key: Symbol): String

  def inferSubcommandName(runtimeClass: Class[_]): String

  def inferSubcommandName(symbol: Symbol): String

  //def readContent[F[_]](application: String, module: String, scope: Scope)(implicit F: Sync[F]): F[String]

  def configObjectSource(scope: Scope): ConfigObjectSource

  def inferConfigNamespace(key: Symbol): String

}

class DefaultBehaviors extends Behaviors {

  def inferOptionName(key: Symbol): String = {
    CaseFormat.LOWER_CAMEL.to(CaseFormat.LOWER_HYPHEN, key.name)
  }

  def inferSubcommandName(runtimeClass: Class[_]): String = {
    CaseFormat.UPPER_CAMEL.to(CaseFormat.LOWER_HYPHEN, runtimeClass.getSimpleName.toLowerCase)
  }

  def inferSubcommandName(symbol: Symbol): String = {
    CaseFormat.UPPER_CAMEL.to(CaseFormat.LOWER_HYPHEN, symbol.name)
  }

  override def inferConfigNamespace(key: Symbol): String = {
    inferOptionName(key)
  }

  override def configObjectSource(scope: Scope): ConfigObjectSource = ConfigSource.empty

}

trait BehaviorsInstances {

  implicit def behaviorsForAny: Behaviors = new DefaultBehaviors

}
