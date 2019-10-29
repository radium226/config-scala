package com.github.radium226.config

import shapeless._

class CoproductSpec extends AbstractSpec {

  type StringOrInt = String :+: Int :+: CNil

  type StringAndInt = String :: Int :: HNil

  it should "be able to have a coproduct" in {
    val stringAndInt = "Zero" :: 0 :: HNil
  }

}
