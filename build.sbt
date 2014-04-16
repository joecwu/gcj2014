name := "google-code-jam-q"

version := "1.0"

scalaVersion := "2.10.2"

//javacOptions ++= Seq("-source", "1.6", "-target", "1.6")

//autoScalaLibrary := false

//scalaHome := Some(file("/Users/joec_wu/tools/scala-2.10.1"))

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

seq(com.github.retronym.SbtOneJar.oneJarSettings: _*)

libraryDependencies += "commons-lang" % "commons-lang" % "2.6"

//libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.1.4"

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test"

libraryDependencies += "org.scala-lang.modules" %% "scala-async" % "0.9.0-M4"

libraryDependencies += "org.clapper" % "grizzled-slf4j_2.10" % "1.0.1"

libraryDependencies += "org.slf4j" % "slf4j-api" % "1.7.1"

libraryDependencies += "org.slf4j" % "log4j-over-slf4j" % "1.7.1"

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.0.1"

//atmosSettings
