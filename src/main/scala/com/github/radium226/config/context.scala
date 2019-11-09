package com.github.radium226.config

import shapeless._


case class Context(
  application: Application
)

trait LookUpContext[T] {

  def apply(): Context

}

object LookUpContext {

  implicit def default[T](implicit
    annotation: Annotation[application, T]
  ): LookUpContext[T] = { () =>
    Context(
      annotation().value
    )
  }

}
