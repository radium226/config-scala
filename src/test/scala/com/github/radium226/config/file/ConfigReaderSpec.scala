package com.github.radium226.config.file

import com.github.radium226.config.AbstractSpec
import pureconfig.ConfigSource

class ConfigReaderSpec extends AbstractSpec {


  case class Settings(minSize: Int, maxSize: Option[Int])

  it should "be able to parse option" in {
    ConfigSource.string("min-size = 2").loadOrThrow[Settings] should be(Settings(2, None))
  }


  sealed trait Action
  case class Create(name: String) extends Action
  case class Delete(id: Int) extends Action

  it should "be able to parse action" in {
    ConfigSource.string(
      """create.name = 2
        |delete.id = 3
        |""".stripMargin
    ).loadOrThrow[Create]

  }

}
