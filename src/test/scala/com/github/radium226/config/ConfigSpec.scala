package com.github.radium226.config

import cats.effect._
import Scope._

class ConfigSpec extends AbstractSpec {

  implicit def configBehaviors: Behaviors = new DefaultBehaviors {

    override def readContent[F[_]](application: String, module: String, scope: Scope)(implicit F: Sync[F]): F[String] = {
      F.pure(scope match {
        case System =>
          """min-size: 2
            |max-size: 2
            |""".stripMargin

        case User =>
          """min-size: 1
            |avg-size: 3
            |""".stripMargin

        case Current =>
          """abs-size: 2
            |""".stripMargin
      })
    }

  }

  case class SimpleSettings(minSize: Int, maxSize: Int, avgSize: Int, absSize: Int)

  it should "be able to parse simple settings" in {
    val settings = Config[IO, SimpleSettings].parse("--abs-size=17").unsafeRunSync()
    settings should be(SimpleSettings(1, 2, 3, 17))
  }


  /*sealed trait Action
  case class Create() extends Action

  case class SettingsWithAction(dryRun: String, action: Action)

  it should "be able to parse settings with action" in {
    val settings = Config[IO, SettingsWithAction].parse("--dry-run", "create").unsafeRunSync()
    println(settings)
  }*/

}