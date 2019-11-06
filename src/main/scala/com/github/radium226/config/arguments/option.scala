package com.github.radium226.config.arguments

import com.monovore.decline._
import shapeless._
import shapeless.labelled._

import scala.reflect._
import cats.implicits._
import com.github.radium226.config.{Behaviors, Piece, debug}
import pureconfig.{ConfigReader, ConfigSource, Derivation}


trait MakeOption[A] {

  def apply(configSource: ConfigSource): Opts[A]

}

trait MakeOptionSubcommandInstances {

  implicit def makeOptionForSubcommandOfAny[K <: Symbol, A](implicit
    makeSubcommandForA: MakeSubcommand.Aux[A],
    witnessForK: Witness.Aux[K],
    behaviors: Behaviors,
    classTagForA: ClassTag[A]
  ): MakeOption.Aux[FieldType[K, A]] = MakeOption.instance({ configSource =>
    debug(s"makeOptionForSubcommandOfAny[${witnessForK.value}, ${classTagForA.runtimeClass.getSimpleName}]")
    val key = witnessForK.value
    val namespace = behaviors.inferConfigNamespace(key)
    makeSubcommandForA(configSource.at(namespace)).map(field[K](_))
  })

}

trait MakeOptionAnyInstances extends MakeOptionSubcommandInstances {

  implicit def makeOptionForAny[K <: Symbol, A](implicit
    argumentForA: Argument[A],
    witnessForK: Witness.Aux[K],
    behaviors: Behaviors,
    classTagForA: ClassTag[A]
  ): MakeOption.Aux[FieldType[K, A]] = MakeOption.instance({ configSource =>
    val name = behaviors.inferOptionName(witnessForK.value)
    debug(s"makeOptionForAny[${witnessForK.value}, ${classTagForA.runtimeClass.getSimpleName}]")
    Opts.option[A](name, "No help! ").map(field[K](_))
  })

}

trait FuckThat extends MakeOptionAnyInstances {

  implicit def makeOptionForSubcommandOfAny2[K <: Symbol, A](implicit
    makeSubcommandForA: MakeSubcommand.Aux[A],
    witnessForK: Witness.Aux[K],
    behaviors: Behaviors,
    classTagForA: ClassTag[A]
  ): MakeOption.Aux[FieldType[K, Option[A]]] = MakeOption.instance({ configSource =>
    debug(s"makeOptionForSubcommandOfAny2[${witnessForK.value}, ${classTagForA.runtimeClass.getSimpleName}]")
    val key = witnessForK.value
    val namespace = behaviors.inferConfigNamespace(key)
    makeSubcommandForA(configSource.at(namespace)).orNone.map(field[K](_))
  })

}

trait MakeOptionHListInstances extends FuckThat {

  implicit def makeOptionForHNil: MakeOption.Aux[HNil] = MakeOption.constant(Opts.unit.as(HNil))

  implicit def makeOptionForHCons[K <: Symbol, H, T <: HList](implicit
    makeOptionForH: MakeOption.Aux[H],
    makeOptionForT: MakeOption.Aux[T],
  ): MakeOption.Aux[H :: T] = MakeOption.instance({ configSource =>
    (makeOptionForH(configSource), makeOptionForT(configSource)).mapN(_ :: _)
  })

  implicit def makeOptionForLabelledGeneric[A, ReprOfA <: HList](implicit
    labelledGeneric: LabelledGeneric.Aux[A, ReprOfA],
    makeOptionForReprOfA: MakeOption.Aux[ReprOfA]
  ): MakeOption.Aux[A] = MakeOption.instance({ configSource =>
    makeOptionForReprOfA(configSource).map(labelledGeneric.from(_))
  })

  implicit def makeOptionForOption[K <: Symbol, A](implicit
    makeOptionForA: MakeOption.Aux[FieldType[K, A]],
    witnessForK: Witness.Aux[K],
    derivationForConfigReaderForOptionOfA: Derivation[ConfigReader[Option[A]]],
    behaviors: Behaviors
  ): MakeOption.Aux[FieldType[K, Option[A]]] = MakeOption.instance({ configSource =>
    val key = witnessForK.value
    val namespace = behaviors.inferConfigNamespace(key)
    makeOptionForA(configSource)
        .map(_.some)
        .orElse(configSource.at(namespace).load[Option[A]].map(Opts(_)).getOrElse(Opts(none[A])))
        .map(field[K](_))
  })

  implicit def makeOptionForBoolean[K <: Symbol](implicit
    witnessForK: Witness.Aux[K],
    behaviors: Behaviors
  ): MakeOption.Aux[FieldType[K, Boolean]] = MakeOption.instance({ configSource =>
    Opts
      .flag(behaviors.inferOptionName(witnessForK.value), "No help! ")
      .map({ _ => true })
      .withDefault(false)
      .map(field[K](_))
  })

}

trait MakeOptionInstances extends MakeOptionHListInstances {

  /*implicit def makeOptionForSubcommandAndOptionOfAny[K <: Symbol, A](implicit
    makeSubcommandForA: MakeSubcommand.Aux[A],
    witnessForK: Witness.Aux[K],
    behaviors: Behaviors,
    classTagForA: ClassTag[A]
  ): MakeOption.Aux[FieldType[K, Option[A]]] = MakeOption.instance({ configSource =>
    debug(s"makeOptionForSubcommandAndOptionOfAny[${witnessForK.value}, ${classTagForA.runtimeClass.getSimpleName}]")
    val key = witnessForK.value
    val namespace = behaviors.inferConfigNamespace(key)
    makeSubcommandForA(configSource.at(namespace)).map(_.some).map(field[K](_))
  })*/

}

trait MakeOptionSyntax {

  def makeOption[A](configSource: ConfigSource)(implicit makeOptionForA: MakeOption[A]): Opts[A] = {
    makeOptionForA.apply(configSource)
  }

  def makeOption[A](implicit makeOptionForA: MakeOption[A]): Opts[A] = {
    makeOption(ConfigSource.empty)
  }

}

object MakeOption {

  type Aux[A] = MakeOption[A]

  def instance[A](f: ConfigSource => Opts[A]): MakeOption.Aux[A] = new MakeOption[A] {

    def apply(configSource: ConfigSource): Opts[A] = f(configSource)

  }

  def constant[A](a: Opts[A]): MakeOption.Aux[A] = MakeOption.instance({ _ => a })

}
