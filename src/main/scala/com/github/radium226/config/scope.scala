package com.github.radium226.config

import java.nio.file.{Path, Paths}

import enumeratum._

sealed trait Scope extends EnumEntry

object Scope extends Enum[Scope] {

  val values = findValues


  case object Current extends Scope

  case object User extends Scope

  case object System extends Scope

}