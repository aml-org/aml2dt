name := "aml2dt"

version := "0.1"

scalaVersion := "2.12.0"

resolvers +=
  "MuleSoft releases" at "https://repository-master.mulesoft.org/nexus/content/repositories/releases"

libraryDependencies += "com.github.amlorg" %% "amf-client" % "3.4.1"
libraryDependencies += "io.circe" %% "circe-parser" % "0.11.1"
libraryDependencies +=  "org.scalatest" %% "scalatest" % "3.0.5" % Test