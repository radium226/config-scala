package com.github.radium226.config.arguments

import cats.effect.Sync
import shapeless._
import com.monovore.decline._

import scala.reflect._
import cats.implicits._
import com.github.radium226.config.Behaviors
import com.github.radium226.config.debug
import com.typesafe.config.ConfigRenderOptions
import pureconfig.ConfigSource
import shapeless.labelled._


trait MakeSubcommand[A] {

  def apply(configSource: ConfigSource): Opts[A]

}

object MakeSubcommand {

  type Aux[A] = MakeSubcommand[A]

  def instance[A](f: ConfigSource => Opts[A]): MakeSubcommand.Aux[A] = new MakeSubcommand[A] {

    def apply(configSource: ConfigSource): Opts[A] = f(configSource)

  }

  def constant[A](a: Opts[A]): MakeSubcommand.Aux[A] = MakeSubcommand.instance({ _ => a })

}

trait MakeSubcommandCoproductInstances {

  implicit def makeSubcommandForCNil: MakeSubcommand.Aux[CNil] = MakeSubcommand.constant(Opts.never)

  implicit def makeSubcommandForCCons[K <: Symbol, H, T <: Coproduct](implicit
    makeOptionForH: MakeOption.Aux[H],
    makeSubcommandForT: MakeSubcommand.Aux[T],
    classTagForH: ClassTag[H],
    witnessForK: Witness.Aux[K],
    behavior: Behaviors,
  ): MakeSubcommand.Aux[FieldType[K, H] :+: T] = MakeSubcommand.instance({ configSource =>
    debug(s"makeSubcommandForCCons[${classTagForH.runtimeClass.getSimpleName}, ...]")
    debug(s"k=${witnessForK.value}")
    val optsForH = {
      val name = behavior.inferSubcommandName(witnessForK.value)
      val opts = makeOptionForH(configSource.at(behavior.inferConfigNamespace(witnessForK.value)))
      Opts.subcommand[H](name, "No help! ")(opts)
    }
    val optsForT = makeSubcommandForT(configSource)
    debug(s"[makeSubcommandForCCons] configSource=${configSource.value().map(_.render(ConfigRenderOptions.concise()))}")
    optsForH
      .map({ h => Inl[FieldType[K, H], T](field[K](h))})
      .orElse(
        optsForT
          .map({ t => Inr[FieldType[K, H], T](t) })
      )
  })

  implicit def makeSubcommandForGeneric[A, ReprOfA <: Coproduct](implicit
    labelledGeneric: LabelledGeneric.Aux[A, ReprOfA],
    makeSubcommandForReprOfA: MakeSubcommand[ReprOfA],
    classTagForA: ClassTag[A]
  ): MakeSubcommand.Aux[A] = MakeSubcommand.instance({ configSource =>
    debug(s"makeSubcommandForGeneric[${classTagForA.runtimeClass.getSimpleName}, ...]")
    makeSubcommandForReprOfA(configSource)
     .map(labelledGeneric.from(_))
  })

}

trait MakeSubcommandInstances extends MakeSubcommandCoproductInstances

trait MakeSubcommandSyntax {

  def makeSubcommand[A](configSource: ConfigSource)(implicit makeSubcommandForA: MakeSubcommand.Aux[A]): Opts[A] = makeSubcommandForA(configSource)

  def makeSubcommand[A](implicit makeSubcommandForA: MakeSubcommand.Aux[A]): Opts[A] = makeSubcommand(ConfigSource.empty)

}
