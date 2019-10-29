package com.github.radium226.config.arguments

import com.monovore.decline._

import com.github.radium226.config.debug


trait MakeCommand[A] {

  def apply(): Command[A]

}

trait MakeCommandInstances {

  implicit def makeCommandForAny[A](implicit
    makeOptionForA: MakeOption[A]
  ): MakeCommand[A] = new MakeCommand[A] {

    override def apply(): Command[A] = Command(name = "No name!", header = "No header! ")(makeOptionForA())

  }

}
