package com.github.radium226.config

import java.nio.file.{Path, Paths}

import cats.effect.Sync
import com.google.common.base.CaseFormat
import pureconfig.{ConfigObjectSource, ConfigSource}

import java.lang.{ System => JavaSystem }

import scala.io.Source

trait Behaviors {

  def inferOptionName(key: Symbol): String

  def inferSubcommandName(runtimeClass: Class[_]): String

  def inferSubcommandName(symbol: Symbol): String

  //def readContent[F[_]](application: String, module: String, scope: Scope)(implicit F: Sync[F]): F[String]

  def configObjectSource(
    application: Application,
    scope: Scope
  ): ConfigObjectSource

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

  override def configObjectSource(
    application: Application,
    scope: Scope
  ): ConfigObjectSource = {
    import Scope._
    ConfigSource.file(scope match {
      case Current =>
        val configFilePath = Paths.get(".").resolve(s"${application}.conf")
        debug(s"configFilePath (Current)=${configFilePath}")
        configFilePath

      case User =>
        val configFilePath = Paths.get(JavaSystem.getProperty("user.home")).resolve(".config").resolve(s"${application}.conf")
        debug(s"configFilePath (User)=${configFilePath}")
        configFilePath

      case System =>
        val configFilePath = Paths.get(s"/etc").resolve(s"${application}.conf")
        debug(s"configFilePath (System)=${configFilePath}")
        configFilePath
    }).recoverWith({ case _ => ConfigSource.empty })
  }

}

object Behaviors {

  implicit def default: Behaviors = new DefaultBehaviors

}
