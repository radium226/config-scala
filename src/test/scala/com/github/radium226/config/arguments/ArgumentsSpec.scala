package com.github.radium226.config.arguments

import cats.effect._
import cats.implicits._

import com.github.radium226.config.{AbstractSpec, Puzzle}
import pureconfig.ConfigSource

import shapeless._


class ArgumentsSpec extends AbstractSpec {

  case class QuiteSimpleSettings(minSize: Int, maxSize: Int)

  it should "be able to parse simple arguments" in {
    val settings = Arguments[IO, QuiteSimpleSettings].parse(ConfigSource.empty,"--min-size=-1", "--max-size=1").unsafeRunSync()
    settings should be(QuiteSimpleSettings(-1, 1))
  }


  case class SettingsWithOption(minSize: Option[Int])

  it should "be able to parse optional arguments" in {
    val arguments = Arguments[IO, SettingsWithOption]

    arguments.parse(ConfigSource.empty).unsafeRunSync() should be(SettingsWithOption(None))
    arguments.parse(ConfigSource.empty,"--min-size=3").unsafeRunSync() should be(SettingsWithOption(Some(3)))
  }


  sealed trait Action
  case class Create(name: String) extends Action
  case class Delete(id: Int) extends Action

  case class SettingsWithAction(dryRun: Boolean, action: Action)

  it should "be able to parse subcommands" in {
    val arguments = Arguments[IO, SettingsWithAction]
    arguments.parse(ConfigSource.empty, "--dry-run", "create", "--name=John").unsafeRunSync() should be(SettingsWithAction(true, Create("John")))
  }


  case class Phone(id: Option[Int], model: Option[String])

  it should "be able to use config if needed" in {
    val configSource = ConfigSource.string("model = 'OnePlus'")
    val phone = Arguments[IO, Phone].parse(configSource, "--id=2").unsafeRunSync()
    println(phone)
  }


  val puzzle = Puzzle[IO, SettingsWithAction]
  type PiecesOfSettingsWithAction = puzzle.Pieces

  it should "be able to parse subcommands and use config if needed" in {
    val configSource = ConfigSource
      .string(
        """action.create.name = "John Doe"
          |action.delete.id = 2
          |dry-run = false
          |""".stripMargin
      )
    //val piecesOfSettingsWithAction = Arguments[IO, PiecesOfSettingsWithAction].parse(configSource, "--dry-run=true", "create").unsafeRunSync()
    //println(piecesOfSettingsWithAction)

    println(puzzle.shuffle(SettingsWithAction(true, Create("toto"))).unsafeRunSync())
  }

  case class Update(id: Option[Int], name: Option[String])
  case class Prune()

  it should "be able to find arguments for Coproduct" in {
    val puzzle = Puzzle[IO, Prune :+: Update :+: CNil]
    println(Coproduct[Prune :+: Update :+: CNil](Update(2.some, "name".some)))
    println(puzzle.shuffle(Coproduct[Prune :+: Update :+: CNil](Update(2.some, "name".some))).unsafeRunSync())
    //val arguments = implicitly[MakeSubcommand[puzzle.Pieces]]
    //println(arguments.apply(ConfigSource.empty))
  }

}
