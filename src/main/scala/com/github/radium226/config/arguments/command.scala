package com.github.radium226.config.arguments

import com.monovore.decline._
import com.github.radium226.config.debug
import pureconfig.ConfigSource


trait MakeCommand[A] {

  def apply(configSource: ConfigSource): Command[A]

}

trait MakeCommandInstances {

  implicit def makeCommandForAny[A](implicit
    makeOptionForA: MakeOption[A]
  ): MakeCommand[A] = new MakeCommand[A] {

    override def apply(configSource: ConfigSource): Command[A] = Command(name = "No name!", header = "No header! ")(makeOptionForA(configSource))

  }

}
