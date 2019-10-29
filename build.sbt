import sbt.Keys.libraryDependencies

ThisBuild / organization := "com.github.radium226"
ThisBuild / scalaVersion := "2.13.1"
ThisBuild / version      := "0.1-SNAPSHOT"

ThisBuild / scalacOptions ++= Seq(
  "-feature",
  "-deprecation",
  "-unchecked",
  "-language:implicitConversions",
  "-language:higherKinds",
  "-language:existentials",
  "-language:postfixOps"
  )

lazy val root = (project in file("."))
  .settings(
    name := "config",

    // Cats
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "2.0.0",
      "org.typelevel" %% "cats-effect" % "2.0.0"
    ),

    // ScalaTest
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.0.8" % "test",
      "org.scalactic" %% "scalactic" % "3.0.8"
    ),

    // Shapeless
    libraryDependencies ++= Seq(
      "com.chuusai" %% "shapeless" % "2.3.3"
    ),

    // Decline
    libraryDependencies ++= Seq(
      "com.monovore" %% "decline" % "1.0.0"
    ),

    // Guava
    libraryDependencies ++= Seq(
      "com.google.guava" % "guava" % "28.1-jre"
    ),

    // PureConfig
    libraryDependencies ++= Seq(
      "com.github.pureconfig" %% "pureconfig" % "0.12.1"
    ),

    // Enumeratum
    libraryDependencies ++= Seq(
      "com.beachape" %% "enumeratum" % "1.5.13"
    ),

    addCompilerPlugin("org.typelevel" % "kind-projector_2.13.1" % "0.11.0")
  )
