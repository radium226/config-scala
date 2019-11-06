package com.github.radium226.config

import cats.effect._
import Scope._
import com.github.radium226.config.arguments.{Arguments, MakeOption}
import pureconfig.ConfigSource

class ConfigSpec extends AbstractSpec {

  implicit def configBehaviors: Behaviors = new DefaultBehaviors {

    override def configObjectSource(scope: Scope) = {
      ConfigSource.string(scope match {
        case System =>
          """min-size: 2
            |max-size: 2
            |action.delete.id=2
            |action.create.toto='kjkjf'
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


  sealed trait Action
  case class Create() extends Action
  case class Delete(id: Int) extends Action

  case class SettingsWithAction(dryRun: Boolean, force: Option[Int], action: Action)

  it should "be able to parse settings with action" in {
    println(Config[IO, SettingsWithAction].parse("--dry-run", "create").unsafeRunSync())
    //println(Config[IO, SettingsWithAction].parse("--help").unsafeRunSync())
    println(Config[IO, SettingsWithAction].parse("delete").unsafeRunSync())
  }

}