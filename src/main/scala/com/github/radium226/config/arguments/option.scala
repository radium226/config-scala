package com.github.radium226.config.arguments

import com.monovore.decline._
import shapeless._
import shapeless.labelled._

import scala.reflect._
import cats.implicits._
import com.github.radium226.config.Behaviors

import com.github.radium226.config.debug


trait MakeOption[A] {

  def apply(): Opts[A]

}

trait MakeOptionLowestPriorityInstances {

  implicit def makeOptionForSubcommandOfAny[K <: Symbol, A](implicit
    makeSubcommandForA: MakeSubcommand.Aux[A],
    witnessForK: Witness.Aux[K],
    behaviors: Behaviors,
    classTagForA: ClassTag[A]
  ): MakeOption.Aux[FieldType[K, A]] = MakeOption.instance({
    debug(s"makeOptionForSubcommandOfAny[${witnessForK.value}, ${classTagForA.runtimeClass.getSimpleName}]")
    makeSubcommandForA().map(field[K](_))
  })

}

trait MakeOptionLowPriorityInstances extends MakeOptionLowestPriorityInstances {

  implicit def makeOptionForAny[K <: Symbol, A](implicit
    argumentForA: Argument[A],
    witnessForK: Witness.Aux[K],
    behaviors: Behaviors,
    classTagForA: ClassTag[A]
  ): MakeOption.Aux[FieldType[K, A]] = MakeOption.instance({
    val name = behaviors.inferOptionName(witnessForK.value)
    debug(s"makeOptionForAny[${witnessForK.value}, ${classTagForA.runtimeClass.getSimpleName}]")
    Opts.option[A](name, "No help! ").map(field[K](_))
  })

}

trait MakeOptionInstances extends MakeOptionLowPriorityInstances {

  implicit def makeOptionForHNil: MakeOption.Aux[HNil] = MakeOption.constant(Opts.unit.as(HNil))

  implicit def makeOptionForHCons[K <: Symbol, H, T <: HList](implicit
    makeOptionForH: MakeOption.Aux[H],
    makeOptionForT: MakeOption.Aux[T],
  ): MakeOption.Aux[H :: T] = MakeOption.instance({
    (makeOptionForH(), makeOptionForT()).mapN(_ :: _)
  })

  implicit def makeOptionForLabelledGeneric[A, ReprOfA <: HList](implicit
    labelledGeneric: LabelledGeneric.Aux[A, ReprOfA],
    makeOptionForReprOfA: MakeOption.Aux[ReprOfA]
  ): MakeOption.Aux[A] = MakeOption.instance({
    makeOptionForReprOfA().map(labelledGeneric.from(_))
  })

  implicit def makeOptionForOption[K <: Symbol, A](implicit
    makeOptionForA: MakeOption.Aux[FieldType[K, A]],
    witnessForK: Witness.Aux[K]
  ): MakeOption.Aux[FieldType[K, Option[A]]] = MakeOption.instance({
    makeOptionForA().orNone.map(field[K](_))
  })

  implicit def makeOptionForBoolean[K <: Symbol](implicit
    witnessForK: Witness.Aux[K],
    behaviors: Behaviors
  ): MakeOption.Aux[FieldType[K, Boolean]] = MakeOption.instance({
    Opts
      .flag(behaviors.inferOptionName(witnessForK.value), "No help! ")
      .map({ _ => true })
      .withDefault(false)
      .map(field[K](_))
  })

}

trait MakeOptionSyntax {

  def makeOption[A](implicit makeOptionForA: MakeOption[A]): Opts[A] = {
    makeOptionForA.apply()
  }

}

object MakeOption {

  type Aux[A] = MakeOption[A]

  def instance[A](f: => Opts[A]): MakeOption.Aux[A] = new MakeOption[A] {

    def apply() = f

  }

  def constant[A](a: Opts[A]): MakeOption.Aux[A] = MakeOption.instance(a)

}
