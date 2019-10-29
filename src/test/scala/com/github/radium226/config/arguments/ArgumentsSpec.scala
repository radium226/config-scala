package com.github.radium226.config.arguments

import cats.effect._
import com.github.radium226.config.AbstractSpec

class ArgumentsSpec extends AbstractSpec {

  case class QuiteSimpleSettings(minSize: Int, maxSize: Int)

  it should "be able to parse simple arguments" in {
    val settings = Arguments[IO, QuiteSimpleSettings].parse("--min-size=-1", "--max-size=1").unsafeRunSync()
    settings should be(QuiteSimpleSettings(-1, 1))
  }


  case class SettingsWithOption(minSize: Option[Int])

  it should "be able to parse optional arguments" in {
    val arguments = Arguments[IO, SettingsWithOption]

    arguments.parse().unsafeRunSync() should be(SettingsWithOption(None))
    arguments.parse("--min-size=3").unsafeRunSync() should be(SettingsWithOption(Some(3)))
  }


  sealed trait Action
  case class Create(name: String) extends Action
  case class Delete(id: Int) extends Action

  case class SettingsWithAction(dryRun: Boolean, action: Action)

  it should "be able to parse subcommands" in {


    val arguments = Arguments[IO, SettingsWithAction]

    arguments.parse("--dry-run", "create", "--name=John").unsafeRunSync() should be(SettingsWithAction(true, Create("John")))
  }

}
