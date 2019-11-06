package com.github.radium226.config.file

import com.google.common.base.CaseFormat
import pureconfig.{ConfigReader, Derivation, error}
import shapeless._
import shapeless.labelled._
import cats.implicits._
import com.github.radium226.config.{Behaviors, Piece}
import pureconfig.error.{ConfigReaderFailure, ConfigReaderFailures, ConfigValueLocation, ThrowableFailure}

import scala.reflect.ClassTag
import com.github.radium226.config.debug
import pureconfig.ConfigReader.Result

trait ConfigReaderHListInstances {

  implicit def configReaderForHNil: ConfigReader[HNil] = {
    ConfigReader.fromCursor(_ => HNil.asRight)
  }

  implicit def configReaderForHCons[K <: Symbol, H, T <: HList](implicit
    configReaderForH: ConfigReader[H],
    configReaderForT: ConfigReader[T],
    witnessForK: Witness.Aux[K],
  ): ConfigReader[FieldType[K, H] :: T] = {
    val keyName = witnessForK.value.name
    println(s"I'm here for ${keyName}")
    for {
      head <- ConfigReader.fromCursor[H]({ cursor =>
        for {
          objectCursor <- cursor.asObjectCursor
          keyCursor    <- objectCursor.atKey(CaseFormat.LOWER_CAMEL.to(CaseFormat.LOWER_HYPHEN, keyName))
          head         <- configReaderForH.from(keyCursor.value)
        } yield head
      })
      tail <- configReaderForT
    } yield field[K](head) :: tail
  }

  implicit def configReaderForGeneric[A, ReprOfA <: HList](implicit
    labelledGeneric: LabelledGeneric.Aux[A, ReprOfA],
    configReaderForReprOfA: ConfigReader[ReprOfA]
  ): ConfigReader[A] = configReaderForReprOfA.map(labelledGeneric.from(_))

}

trait ConfigReaderPieceInstances extends ConfigReaderHListInstances {

  implicit def configReaderForPieceHCons[K <: Symbol, H, T <: HList](implicit
    configReaderForH: ConfigReader[H],
    configReaderForT: ConfigReader[T],
    witnessForK: Witness.Aux[K],
  ): ConfigReader[FieldType[K, Piece[H]] :: T] = {
    val keyName = witnessForK.value.name
    debug(s"configReaderForPieceHCons")
    for {
      head <- ConfigReader.fromCursor[Option[H]]({ cursor =>
        for {
          objectCursor <- cursor.asObjectCursor
          head         <- Option(objectCursor.atKeyOrUndefined(CaseFormat.LOWER_CAMEL.to(CaseFormat.LOWER_HYPHEN, keyName)).value).traverse(configReaderForH.from(_))
        } yield head
      })
      tail <- configReaderForT
    } yield field[K](head) :: tail
  }

}

trait ConfigReaderInstances extends ConfigReaderPieceInstances