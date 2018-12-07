name := "marsRover"

version := "1.0"

scalaVersion := "2.12.2"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.5"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"
libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.23"

scalacOptions in Compile := (scalacOptions in Compile).value.filter(_ != "-Yinline-warnings")
