package com.github.radium226.config

import cats.effect._
import cats.implicits._

import shapeless._
import shapeless.syntax.singleton._


class PuzzleSpec extends AbstractSpec {

  case class Simple(minSize: Int, maxSize: Int)

  val simplePuzzle  = Puzzle[IO, Simple]

  type SimplePieces = simplePuzzle.Pieces

  it should "be able to shuffle simple case class" in {
    val simple = Simple(2, 4)
    val piecesOfSettings = Symbol("minSize") ->> 2.some :: Symbol("maxSize") ->> 4.some ::HNil
    simplePuzzle.shuffle(simple).unsafeRunSync() should be(piecesOfSettings)
  }

  it should "not be able to assemble settings if pieces are missing" in {
    val simplePieces = (Symbol("minSize") ->> 2.some :: Symbol("maxSize") ->> none ::HNil).asInstanceOf[SimplePieces]
    an[Exception] should be thrownBy {
      simplePuzzle.assemble(simplePieces).unsafeRunSync()
    }
  }

  it should "be able to assemble settings if all pieces are here" in {
    val piecesOfSettings = (Symbol("minSize") ->> 2.some :: Symbol("maxSize") ->> 3.some ::HNil).asInstanceOf[SimplePieces]
    simplePuzzle.assemble(piecesOfSettings).unsafeRunSync() should be(Simple(2, 3))
  }


  case class Range(minSize: Int, maxSize: Int)
  case class Complex(dryRun: Boolean, range: Range)

  val complexPuzzle = Puzzle[IO, Complex]

  type ComplexPieces = complexPuzzle.Pieces

  it should "be able to shuffle complex case class" in {
    //println(Typeable[complexPuzzle.Pieces].describe)
    val complexPieces = complexPuzzle.shuffle(Complex(true, Range(-1, 1))).unsafeRunSync()
    println(complexPieces)
  }


  sealed trait Food
  case class Potatoes(count: Int) extends Food
  case class Tomatoes(count: Int) extends Food

  case class Pan(radius: Int, food: Food)

  val panPuzzle  = Puzzle[IO, Pan]
  type PanPieces = panPuzzle.Pieces

  it should "be able to shuffle case with sealed trait" in {
    println(panPuzzle.shuffle(Pan(1, Potatoes(666))).unsafeRunSync())
  }


}
