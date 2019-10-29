package com.github.radium226.config.file

import cats.effect._
import com.github.radium226.config.AbstractSpec

class FileSpec extends AbstractSpec {

  case class SettingsWithoutOptions(minSize: Int, maxSize: Int)

  it should "be able to parse settings without options" in {
    File[IO, SettingsWithoutOptions].parse(
      """
        |min-size = 2
        |max-size = 3
        |""".stripMargin).unsafeRunSync() should be(SettingsWithoutOptions(2, 3))
  }

  case class SettingsWithOptions(minSize: Option[Int], maxSize: Option[Int])

  it should "be able to parse settings with options" in {
    File[IO, SettingsWithOptions].parse("min-size = 2").unsafeRunSync() should be(SettingsWithOptions(Some(2), None))
  }

}
