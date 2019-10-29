package com.github.radium226.config

import cats.effect._
import cats.implicits._

import shapeless._
import shapeless.syntax.singleton._


class PuzzleSpec extends AbstractSpec {

  case class Settings(minSize: Int, maxSize: Int)

  val puzzle  = Puzzle[IO, Settings]


  it should "be able to shuffle settings" in {
    val settings = Settings(2, 4)
    val piecesOfSettings = Symbol("minSize") ->> 2.some :: Symbol("maxSize") ->> 4.some ::HNil
    puzzle.shuffle(settings).unsafeRunSync() should be(piecesOfSettings)
  }

  it should "not be able to assemble settings if pieces are missing" in {
    val piecesOfSettings = (Symbol("minSize") ->> 2.some :: Symbol("maxSize") ->> none ::HNil).asInstanceOf[puzzle.Pieces]
    an[Exception] should be thrownBy {
      puzzle.assemble(piecesOfSettings).unsafeRunSync()
    }
  }

  it should "be able to assemble settings if all pieces are here" in {
    val piecesOfSettings = (Symbol("minSize") ->> 2.some :: Symbol("maxSize") ->> 3.some ::HNil).asInstanceOf[puzzle.Pieces]
    puzzle.assemble(piecesOfSettings).unsafeRunSync() should be(Settings(2, 3))
  }

}
