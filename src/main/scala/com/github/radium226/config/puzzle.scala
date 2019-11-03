package com.github.radium226.config

import cats._
import cats.effect._
import cats.implicits._

import shapeless._
import shapeless.labelled._


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
    F: Sync[F]
  ): Puzzle.Aux[F, A, Piece[A]] = new Puzzle[F, A] {

    type Pieces = Piece[A]

    def shuffle(a: A): F[Pieces] = F.pure(Piece(a))

    def assemble(pieces: Pieces): F[A] = {
      pieces.map(F.pure(_)).getOrElse(F.raiseError(new Exception("There is a missing piece! ")))
    }

  }

}

trait PuzzleHListInstances extends PuzzleDefaultInstances {

  implicit def puzzleForHNil[F[_]](implicit
    F: Sync[F]
  ): Puzzle.Aux[F, HNil, HNil] = new Puzzle[F, HNil] {

    type Pieces = HNil

    def shuffle(HNil: HNil): F[Pieces] = {
      F.pure(HNil)
    }

    def assemble(pieces: Pieces): F[HNil] = {
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
      for {
        piecesForH <- puzzleForH.shuffle(finished.head.asInstanceOf[H])
        piecesForT <- puzzleForT.shuffle(finished.tail)
      } yield field[K](piecesForH) :: piecesForT
    }

    def assemble(pieces: Pieces): F[FieldType[K, H] :: T] = {
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
      puzzleForReprOfA.shuffle(labelledGeneric.to(a))
    }

    def assemble(pieces: Pieces): F[A] = {
      puzzleForReprOfA.assemble(pieces).map(labelledGeneric.from(_))
    }

  }

}

trait PuzzleCoproductInstances extends PuzzleHListInstances {

  implicit def puzzleForCNil[F[_]](implicit
    F: Sync[F]
  ): Puzzle.Aux[F, CNil, CNil] = new Puzzle[F, CNil] {

    type Pieces = CNil

    def shuffle(cNil: CNil): F[Pieces] = {
      F.raiseError(new Exception("CNil! "))
    }

    def assemble(pieces: Pieces): F[CNil] = {
      F.raiseError(new Exception("CNil! "))
    }

  }

  implicit def puzzleForCCons[F[_], K, H, T <: Coproduct, PiecesOfH <: Coproduct, PiecesOfT <: Coproduct](implicit
    F: Sync[F],
    puzzleForH: Puzzle.Aux[F, H, PiecesOfH],
    puzzleForT: Puzzle.Aux[F, T, PiecesOfT],
    witnessForK: Witness.Aux[K]
  ): Puzzle.Aux[F, FieldType[K, H :+: T], FieldType[K, PiecesOfH :+: PiecesOfT]] = new Puzzle[F, FieldType[K, H :+: T]] {

    type Pieces = FieldType[K, PiecesOfH :+: PiecesOfT]

    def shuffle(finished: FieldType[K, H :+: T]): F[Pieces] = {
      (finished.asInstanceOf[H :+: T] match {
        case Inl(h) =>
          puzzleForH
            .shuffle(h)
            .map(_.asInstanceOf[H])
            .map({ piecesOfH =>
              Coproduct(piecesOfH)
            })
        case Inr(t) =>
          puzzleForT
            .shuffle(t)
            .map(_.asInstanceOf[T])
            .map({ piecesOfT =>
              Coproduct(piecesOfT)
            })
      })
        .map(_.asInstanceOf[PiecesOfH :+: PiecesOfT])
        .map({ pieces: (PiecesOfH :+: PiecesOfT) =>
          field[K](pieces).asInstanceOf[FieldType[K, PiecesOfH :+: PiecesOfT]]
        })
    }

    def assemble(pieces: Pieces): F[FieldType[K, H :+: T]] = {
      (pieces.asInstanceOf[PiecesOfH :+: PiecesOfT] match {
        case Inl(piecesOfH) =>
          puzzleForH.assemble(piecesOfH).map(Coproduct(_)).map(_.asInstanceOf[H :+: T])
        case Inr(piecesOfT) =>
          puzzleForT.assemble(piecesOfT).map(Coproduct(_)).map(_.asInstanceOf[H :+: T])
      }).map({ finished => field[K](finished) })
    }

  }

  implicit def puzzleForCoproduct[F[_], K, A, ReprOfA <: Coproduct, PiecesOfReprOfA](implicit
    F: Sync[F],
    generic: Generic.Aux[A, ReprOfA],
    //puzzleForReprOfA: Puzzle.Aux[F, FieldType[K, ReprOfA], FieldType[K, PiecesOfReprOfA]],
    //witnessForK: Witness.Aux[K],
  ): Puzzle.Aux[F, FieldType[K, A], FieldType[K, PiecesOfReprOfA]] = new Puzzle[F, FieldType[K, A]] {

    type Pieces = FieldType[K, PiecesOfReprOfA]

    def shuffle(finished: FieldType[K, A]): F[Pieces] = {
      ??? //puzzleForReprOfA.shuffle(field[K](generic.to(finished.asInstanceOf[A])))
    }

    def assemble(pieces: Pieces): F[FieldType[K, A]] = {
      ??? /*puzzleForReprOfA
        .assemble(pieces)
        .map(_.asInstanceOf[ReprOfA])
        .map({ reprOfA => field[K](generic.from(reprOfA)) })*/
    }

  }

}

trait PuzzleInstances extends PuzzleCoproductInstances
