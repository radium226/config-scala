package com.github.radium226.config.arguments

import cats.effect.Sync
import shapeless._
import com.monovore.decline._

import scala.reflect._

import cats.implicits._

import com.github.radium226.config.Behaviors

import com.github.radium226.config.debug


trait MakeSubcommand[A] {

  def apply(): Opts[A]

  def instance[A](f: => Opts[A]): MakeSubcommand.Aux[A] = new MakeSubcommand[A] {

    def apply() = f

  }

}

object MakeSubcommand {

  type Aux[A] = MakeSubcommand[A]

  def instance[A](f: => Opts[A]): MakeSubcommand.Aux[A] = new MakeSubcommand[A] {

    def apply(): Opts[A] = f

  }

  def constant[A](a: Opts[A]): MakeSubcommand.Aux[A] = MakeSubcommand.instance(a)

}

trait MakeSubcommandLowPriorityInstances {

  implicit def makeSubcommandForAny[A](implicit
    makeOptionForA: Lazy[MakeOption[A]],
    classTagForA: ClassTag[A],
    behavior: Behaviors
  ): MakeSubcommand.Aux[A] = MakeSubcommand.instance({
    val name = behavior.inferSubcommandName(classTagForA.runtimeClass)
    val opts = makeOptionForA.value()
    Opts.subcommand[A](name, "No help! ")(opts)
  })

}

trait MakeSubcommandInstances extends MakeSubcommandLowPriorityInstances {

  implicit def makeSubcommandForCNil[K <: Symbol]: MakeSubcommand.Aux[CNil] = MakeSubcommand.constant(Opts.never)

  implicit def makeSubcommandForCCons[H, T <: Coproduct](implicit
    makeSubcommandForH: MakeSubcommand.Aux[H],
    makeSubcommandForT: MakeSubcommand.Aux[T],
    classTagForH: ClassTag[H]
  ): MakeSubcommand.Aux[H :+: T] = MakeSubcommand.instance({
    debug(s"makeSubcommandForCCons[${classTagForH.runtimeClass.getSimpleName}, ...]")
    val optsForH = makeSubcommandForH()
    val optsForT = makeSubcommandForT()
    optsForH.map(Coproduct(_)).orElse(optsForT.map(Coproduct(_)))
      .map(_.asInstanceOf[H :+: T])
  })

  implicit def makeSubcommandForGeneric[A, ReprOfA <: Coproduct](implicit
    generic: Generic.Aux[A, ReprOfA],
    makeSubcommandForReprOfA: MakeSubcommand[ReprOfA],
    classTagForA: ClassTag[A]
  ): MakeSubcommand.Aux[A] = MakeSubcommand.instance({
    debug(s"makeSubcommandForGeneric[${classTagForA.runtimeClass.getSimpleName}, ...]")
    makeSubcommandForReprOfA()
        .map(generic.from(_))
  })

}

trait MakeSubcommandSyntax {

  def makeSubcommand[A](implicit makeSubcommandForA: MakeSubcommand.Aux[A]): Opts[A] = makeSubcommandForA()

}
