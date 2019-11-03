package com.github.radium226.config.arguments

import cats.effect.Sync
import shapeless._
import com.monovore.decline._

import scala.reflect._
import cats.implicits._
import com.github.radium226.config.Behaviors
import com.github.radium226.config.debug
import pureconfig.ConfigSource


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

trait MakeSubcommandLowPriorityInstances {

  implicit def makeSubcommandForAny[A](implicit
    makeOptionForA: Lazy[MakeOption[A]],
    classTagForA: ClassTag[A],
    behavior: Behaviors
  ): MakeSubcommand.Aux[A] = MakeSubcommand.instance({ configSource =>
    val name = behavior.inferSubcommandName(classTagForA.runtimeClass)
    val opts = makeOptionForA.value(configSource)
    Opts.subcommand[A](name, "No help! ")(opts)
  })

}

trait MakeSubcommandInstances extends MakeSubcommandLowPriorityInstances {

  implicit def makeSubcommandForCNil[K <: Symbol]: MakeSubcommand.Aux[CNil] = MakeSubcommand.constant(Opts.never)

  implicit def makeSubcommandForCCons[H, T <: Coproduct](implicit
    makeSubcommandForH: MakeSubcommand.Aux[H],
    makeSubcommandForT: MakeSubcommand.Aux[T],
    classTagForH: ClassTag[H]
  ): MakeSubcommand.Aux[H :+: T] = MakeSubcommand.instance({ configSource =>
    debug(s"makeSubcommandForCCons[${classTagForH.runtimeClass.getSimpleName}, ...]")
    val optsForH = makeSubcommandForH(configSource)
    val optsForT = makeSubcommandForT(configSource)
    optsForH.map(Coproduct(_)).orElse(optsForT.map(Coproduct(_)))
      .map(_.asInstanceOf[H :+: T])
  })

  implicit def makeSubcommandForGeneric[A, ReprOfA <: Coproduct](implicit
    generic: Generic.Aux[A, ReprOfA],
    makeSubcommandForReprOfA: MakeSubcommand[ReprOfA],
    classTagForA: ClassTag[A]
  ): MakeSubcommand.Aux[A] = MakeSubcommand.instance({ configSource =>
    debug(s"makeSubcommandForGeneric[${classTagForA.runtimeClass.getSimpleName}, ...]")
    makeSubcommandForReprOfA(configSource)
        .map(generic.from(_))
  })

}

trait MakeSubcommandSyntax {

  def makeSubcommand[A](configSource: ConfigSource)(implicit makeSubcommandForA: MakeSubcommand.Aux[A]): Opts[A] = makeSubcommandForA(configSource)

  def makeSubcommand[A](implicit makeSubcommandForA: MakeSubcommand.Aux[A]): Opts[A] = makeSubcommand(ConfigSource.empty)

}
