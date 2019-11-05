package com.github.radium226.config.arguments

import cats.effect.IO
import com.github.radium226.config.{AbstractSpec, Puzzle, arguments}
import com.monovore.decline.Opts
import pureconfig.ConfigSource

class MakeOptionSpec extends AbstractSpec {

  case class Simple(int: Int, string: String)

  it should "be able to make an option from a simple case class" in {
    val makeOption = implicitly[MakeOption[Simple]]
    val option = makeOption(ConfigSource.empty)
    println(option)
  }

  sealed trait Sealed
  case class First(int: Int) extends Sealed
  case class Second(string: String) extends Sealed

  it should "be able to make a command from a sealed trait" in {
    val makeSubcommand = implicitly[MakeSubcommand[Sealed]]
    val subcommand = makeSubcommand(ConfigSource.empty)
    println(subcommand)
  }

  trait MakeOptionUsingPieces[A] {

    def apply(): Opts[A]

  }

  object MakeOptionUsingPieces {

    implicit def default[A, PiecesOfA](implicit
      puzzleForA: Puzzle.Aux[IO, A, PiecesOfA],
      makeOptionForPiecesOfA: MakeOption.Aux[PiecesOfA]
    ): MakeOptionUsingPieces[A] = new MakeOptionUsingPieces[A] {

      override def apply(): Opts[A] = {
        makeOptionForPiecesOfA(ConfigSource.empty).map(puzzleForA.assemble(_).unsafeRunSync())
      }

    }

  }

  it should "be able to make option using pieces of simple case class" in {
    val makeOptionUsingPieces = implicitly[MakeOptionUsingPieces[Simple]]
    val optionUsingPieces = makeOptionUsingPieces()
    println(s"optionUsingPieces=${optionUsingPieces}")
  }


  trait MakeSubcommandUsingPieces[A] {

    def apply(): Opts[A]

  }

  object MakeSubcommandUsingPieces {

    implicit def default[A, PiecesOfA](implicit
      puzzleForA: Puzzle.Aux[IO, A, PiecesOfA],
      makeSubcommandForPiecesOfA: MakeSubcommand.Aux[PiecesOfA]
    ): MakeSubcommandUsingPieces[A] = new MakeSubcommandUsingPieces[A] {

      override def apply(): Opts[A] = {
        makeSubcommandForPiecesOfA(ConfigSource.empty).map(puzzleForA.assemble(_).unsafeRunSync())
      }

    }

  }

  it should "be able to make subcommand using pieces of sealed trait" in {
    val makeSubcommandUsingPieces = implicitly[MakeSubcommandUsingPieces[Sealed]]
    val subcommandUsingPieces = makeSubcommandUsingPieces()
    println(s"subcommandUsingPieces=${subcommandUsingPieces}")

  }


  case class Complex(dryRun: Boolean, `sealed`: Sealed)

  it should "be able to make option using pieces of complex clase class" in {
    val makeOptionUsingPieces = implicitly[MakeOptionUsingPieces[Complex]]
    val optionUsingPieces = makeOptionUsingPieces()
    println(s"optionUsingPieces=${optionUsingPieces}")
  }

}
