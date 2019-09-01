package aml.cim

import java.io.{BufferedWriter, File, FileWriter}
import java.nio.file.{Files, Path, Paths}

import amf.core.AMF
import amf.core.model.document.BaseUnit
import amf.core.parser.UnspecifiedReference
import amf.core.remote.{Cache, Context}
import amf.core.services.RuntimeCompiler

import scala.collection.JavaConverters._
import amf.core.unsafe.PlatformSecrets
import amf.plugins.document.vocabularies.AMLPlugin
import amf.plugins.document.vocabularies.model.document.Dialect
import amf.plugins.document.webapi.{Oas20Plugin, Raml10Plugin}
import aml.dt.Generator

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

object RepositoryLoader extends PlatformSecrets {

  def loadDialect(path: String): Future[BaseUnit] = {
    for {
      _      <- {
        AMF.registerPlugin(AMLPlugin)
        AMF.registerPlugin(Raml10Plugin)
        AMF.registerPlugin(Oas20Plugin)
        AMF.init()
      }
      parsed <- RuntimeCompiler(path,
        Some("application/yaml"),
        Some("AML 1.0"),
        Context(platform),
        UnspecifiedReference,
        Cache())
    } yield {
      parsed
    }
  }

  def fromDirectory(path: String) = {
    var files = Files.walk(Paths.get(path)).iterator().asScala
    val schemaFiles = files.filter(f => f.endsWith("schema.yaml"))
    files = Files.walk(Paths.get(path)).iterator().asScala
    schemaFiles.foreach { f =>
      try {
        processSchemaFile(f)
      } catch {
        case e: Exception => {
          println(s"${f} failed!!! ${e.getMessage}")
        }
      }

    }
  }

  protected def processSchemaFile(f: Path) = {
    println(s"*** Loading schema file ${f.toFile.getAbsolutePath}")

    val dialect = Await.result(loadDialect("file://" + f.toFile.getAbsolutePath), Duration.Inf).asInstanceOf[Dialect]
    val generatedRaml = new Generator(dialect).generate("RAML 1.0")
    val generatedJson = new Generator(dialect).generate("JSON-Schema")

    var targetPath = f.toFile.getAbsolutePath.replace("schema.yaml", "schema.raml")
    writeFile(targetPath, generatedRaml)
    targetPath = f.toFile.getAbsolutePath.replace("schema.yaml", "schema.json")
    writeFile(targetPath, generatedJson)
  }

  protected def writeFile(filename: String, text: String) = {
    val file = new File(filename)
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(text)
    bw.close()
  }

  def main(args: Array[String]) = {
    if (args.length != 1) {
      println("Path to a directory containing CIM files must be provided as an argument")
      System.exit(1)
    }
    val path = args(0)
    println(s"\n\nProcessing directory $path\n\n")
    fromDirectory(path)
  }

}
