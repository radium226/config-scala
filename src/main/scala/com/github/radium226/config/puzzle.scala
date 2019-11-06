package com.github.radium226.config

import cats._
import cats.effect._
import cats.implicits._
import shapeless._
import shapeless.labelled._
import shapeless.ops.coproduct.Inject

import scala.reflect.ClassTag


trait Puzzle[F[_], Finished] {

  type Pieces

  def shuffle(finished: Finished): F[Pieces]

  def assemble(pieces: Pieces): F[Finished]

}

object Puzzle {

  type Aux[F[_], A, PiecesOfA] = Puzzle[F, A] { type Pieces = PiecesOfA }

  def apply[F[_], A](implicit puzzleForA: Puzzle[F, A]): Puzzle[F, A] = puzzleForA

}

trait PuzzleDefaultInstances {

  implicit def puzzleForAny[F[_], A](implicit
    F: Sync[F],
    classTagForA: ClassTag[A]
  ): Puzzle.Aux[F, A, Piece[A]] = new Puzzle[F, A] {

    type Pieces = Piece[A]

    def shuffle(a: A): F[Pieces] = {
      debug(s"puzzleForAny[F[_], ${classTagForA.runtimeClass.getSimpleName}].shuffle(${a})")
      F.pure(Piece(a))
    }

    def assemble(pieces: Pieces): F[A] = {
      debug(s"puzzleForAny[F[_], ${classTagForA.runtimeClass.getSimpleName}].assemble(${pieces})")
      pieces.map(F.pure(_)).getOrElse(F.raiseError(new Exception("There is a missing piece! ")))
    }

  }

}

trait PuzzleCoproductInstances extends PuzzleDefaultInstances {

  implicit def puzzleForCNil[F[_]](implicit
    F: Sync[F]
  ): Puzzle.Aux[F, CNil, CNil] = new Puzzle[F, CNil] {

    type Pieces = CNil

    def shuffle(cNil: CNil): F[Pieces] = {
      F.raiseError(new Exception("CNil! "))
    }

    def assemble(pieces: Pieces): F[CNil] = {
      debug(s"puzzleForCNil[F[_], ...).assemble(${pieces})")
      F.raiseError(new Exception("CNil! "))
    }

  }

  implicit def puzzleForCCons[F[_], K <: Symbol, H, T <: Coproduct, PiecesOfH, PiecesOfT <: Coproduct](implicit
    F: Sync[F],
    puzzleForH: Puzzle.Aux[F, H, PiecesOfH],
    puzzleForT: Puzzle.Aux[F, T, PiecesOfT],
    witnessForK: Witness.Aux[K],
    classTagForH: ClassTag[H]
  ): Puzzle.Aux[F, FieldType[K, H] :+: T, FieldType[K, PiecesOfH] :+: PiecesOfT] = new Puzzle[F, FieldType[K, H] :+: T] {

    type Pieces = FieldType[K, PiecesOfH] :+: PiecesOfT

    def shuffle(finished: FieldType[K, H] :+: T): F[Pieces] = {
      println(s" --------> k = ${witnessForK.value}")
      finished match {
        case Inl(h) =>
          puzzleForH
            .shuffle(h)
            .map({ piecesOfH =>
              Inl[FieldType[K, PiecesOfH], PiecesOfT](field[K](piecesOfH))
            })
        case Inr(t) =>
          puzzleForT
            .shuffle(t)
            .map(Inr[FieldType[K, PiecesOfH], PiecesOfT](_))
      }
    }

    def assemble(pieces: Pieces): F[FieldType[K, H] :+: T] = {
      debug(s"puzzleForCCons[F[_], ...).assemble(${pieces}) / classTagForH=${classTagForH.runtimeClass.getSimpleName}")
      pieces match {
        case Inl(piecesOfH) =>
          println(s"piecesOfH=${piecesOfH}")
          puzzleForH
            .assemble(piecesOfH)
            .map({ h =>
              Inl[FieldType[K, H], T](field[K](h))
            })
        case Inr(piecesOfT) =>
          puzzleForT
            .assemble(piecesOfT)
            .map(Inr[FieldType[K, H], T](_))
      }
    }

  }

  implicit def puzzleForCoproduct[F[_], A, ReprOfA <: Coproduct, PiecesOfReprOfA <: Coproduct](implicit
    F: Sync[F],
    classTagForA: ClassTag[A],
    labelledGeneric: LabelledGeneric.Aux[A, ReprOfA],
    puzzleForReprOfA: Puzzle.Aux[F, ReprOfA, PiecesOfReprOfA]
  ): Puzzle.Aux[F, A, Piece[PiecesOfReprOfA]] = new Puzzle[F, A] {

    type Pieces = Piece[PiecesOfReprOfA]

    def shuffle(finished: A): F[Pieces] = {
      debug(s"puzzleForCoproduct[F[_], ...].shuffle(${finished})")
      puzzleForReprOfA
        .shuffle(labelledGeneric.to(finished))
        .map(_.some)
    }

    def assemble(pieces: Pieces): F[A] = {
      debug(s"puzzleForCoproduct[F[_], ...].assemble(${pieces})")
      pieces
        .map({ piecesOfReprOfA =>
          puzzleForReprOfA
            .assemble(piecesOfReprOfA)
            .map(labelledGeneric.from(_))
        })
        .fold(F.raiseError[A](new Exception("Unable to assemble my coproduct")))(identity)
    }

  }

}

trait PuzzleHListInstances extends PuzzleCoproductInstances {

  implicit def puzzleForHNil[F[_]](implicit
    F: Sync[F]
  ): Puzzle.Aux[F, HNil, HNil] = new Puzzle[F, HNil] {

    type Pieces = HNil

    def shuffle(HNil: HNil): F[Pieces] = {
      debug(s"puzzleForHNil[F[_], ...].shuffle(${HNil})")
      F.pure(HNil)
    }

    def assemble(pieces: Pieces): F[HNil] = {
      debug(s"puzzleForHNil[F[_], ...).assemble(${pieces})")
      F.pure(HNil)
    }

  }

  implicit def puzzleForHCons[F[_], K <: Symbol, H, T <: HList, PiecesOfH, PiecesOfT <: HList](implicit
    F: Sync[F],
    puzzleForH: Puzzle.Aux[F, H, PiecesOfH],
    puzzleForT: Puzzle.Aux[F, T, PiecesOfT],
    witnessForK: Witness.Aux[K]
  ): Puzzle.Aux[F, FieldType[K, H] :: T, FieldType[K, PiecesOfH] :: PiecesOfT] = new Puzzle[F, FieldType[K, H] :: T] {

    type Pieces = FieldType[K, PiecesOfH] :: PiecesOfT

    def shuffle(finished: FieldType[K, H] :: T): F[Pieces] = {
      debug(s"puzzleForHCons[F[_], ...].shuffle(${finished})")
      for {
        piecesForH <- puzzleForH.shuffle(finished.head.asInstanceOf[H])
        piecesForT <- puzzleForT.shuffle(finished.tail)
      } yield field[K](piecesForH) :: piecesForT
    }

    def assemble(pieces: Pieces): F[FieldType[K, H] :: T] = {
      debug(s"puzzleForHCons[F[_], ...].assemble(${pieces})")
      for {
        h <- puzzleForH.assemble(pieces.head)
        t <- puzzleForT.assemble(pieces.tail)
      } yield field[K](h) :: t
    }

  }

  implicit def puzzleForHList[F[_], A, ReprOfA <: HList, PiecesOfReprOfA <: HList](implicit
    F: Sync[F],
    labelledGeneric: LabelledGeneric.Aux[A, ReprOfA],
    puzzleForReprOfA: Puzzle.Aux[F, ReprOfA, PiecesOfReprOfA]
  ): Puzzle.Aux[F, A, PiecesOfReprOfA] = new Puzzle[F, A] {

    type Pieces = PiecesOfReprOfA

    def shuffle(a: A): F[Pieces] = {
      debug(s"puzzleForHList[F[_], ...].shuffle(${a})")
      puzzleForReprOfA.shuffle(labelledGeneric.to(a))
    }

    def assemble(pieces: Pieces): F[A] = {
      debug(s"puzzleForHList[F[_], ...].assemble(${pieces})")
      puzzleForReprOfA.assemble(pieces).map(labelledGeneric.from(_))
    }

  }

}

trait PuzzleOptionInstances extends PuzzleHListInstances {

  implicit def puzzleForOption[F[_], A](implicit
    F: Sync[F],
  ): Puzzle.Aux[F, Option[A], Piece[A]] = new Puzzle[F, Option[A]] {

    type Pieces = Piece[A]

    override def assemble(pieces: Pieces): F[Option[A]] = {
      debug(s"puzzleForOption[F[_], ...].assemble(${pieces})")
      F.pure(pieces)
    }

    override def shuffle(finished: Option[A]): F[Piece[A]] = {
      F.pure(finished)
    }

  }

}

trait PuzzleInstances extends PuzzleOptionInstances
