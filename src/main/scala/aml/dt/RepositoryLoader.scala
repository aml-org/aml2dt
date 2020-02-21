package aml.dt

import java.io.{BufferedWriter, File, FileWriter}
import java.nio.file.{Files, Path, Paths}

import amf.core.AMF
import amf.core.model.document.BaseUnit
import amf.core.parser.UnspecifiedReference
import amf.core.remote.{Cache, Context}
import amf.core.services.RuntimeCompiler
import amf.core.unsafe.PlatformSecrets
import amf.plugins.document.vocabularies.AMLPlugin
import amf.plugins.document.vocabularies.model.document.Dialect
import amf.plugins.document.vocabularies.model.domain.NodeMapping
import amf.plugins.document.webapi.{Oas20Plugin, Raml10Plugin}

import scala.collection.JavaConverters._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

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
        Cache())
    } yield {
      parsed
    }
  }

  def generateGlobalSchemas(filesAndDialects: Future[Iterator[Option[(Path, Dialect)]]], path: String) = {
    val result = filesAndDialects map { parsed =>
      val dialects: Seq[(Path, Dialect)] = parsed.collect {
        case Some((f,dialect)) => (f, dialect)
      } toSeq

      val library = Dialect().withId("http://cloudinformationmodel.com/globalSchema").withLocation("schema.yaml")
      dialects.foreach { case (_, dialect) =>
        library.withDeclares(library.declares ++ dialect.declares)
      }

      library.withDeclares(library.declares.sortBy(_.asInstanceOf[NodeMapping].name.value()))

      println(s"*** Generating global schema")
      val generatedRaml = new Generator(library).generate("RAML 1.0")
      val generatedJson = new Generator(library).generate("JSON-Schema")

      val globalRaml: String = new File(path,  "schema.raml").getAbsolutePath
      println(s"*** Writing global RAML types at ${globalRaml}")
      writeFile(globalRaml, generatedRaml)

      val globalJson = new File(path,  "schema.json").getAbsolutePath
      println(s"*** Writing global JSON Schema types at ${globalJson}")
      writeFile(globalJson, generatedJson)

      dialects
    }

    val toReturn = Await.result(result, Duration.Inf)
    toReturn
  }

  def fromDirectory(path: String) = {
    val files = Files.walk(Paths.get(path)).iterator().asScala
    val schemaFiles = files.filter(f => f.endsWith("schema.yaml"))
    val filesAndDialects = Future.sequence(schemaFiles.map(parseSchemaFile))

    val dialects = generateGlobalSchemas(filesAndDialects, path)
    dialects foreach  {
      case (f, dialect) =>
        try {
          processSchemaFile(f, dialect)
          println(s"*** Processed ${f}")
        } catch {
          case e: Exception =>
            println(s"failed!!! ${e.getMessage}")
        }
    }
  }

  protected def parseSchemaFile(f: Path) = {
    println(s"*** Loading schema file ${f.toFile.getAbsolutePath}")
    loadDialect("file://" + f.toFile.getAbsolutePath) map {
      case d: Dialect => Some((f,d))
      case _          => None
    }
  }

  protected def processSchemaFile(f: Path, dialect: Dialect) = {
    println(s"*** Generating schema for file ${f.toFile.getAbsolutePath}")
    val generatedRaml = new Generator(dialect).generate("RAML 1.0")
    val generatedJson = new Generator(dialect).generate("JSON-Schema")

    var targetPath = f.toFile.getAbsolutePath.replace("schema.yaml", "schema.raml")
    writeFile(targetPath, generatedRaml)
    targetPath = f.toFile.getAbsolutePath.replace("schema.yaml", "schema.json")
    writeFile(targetPath, generatedJson)
    //val dialect = Await.result(, Duration.Inf).asInstanceOf[Dialect]
  }

  protected def writeFile(filename: String, text: String) = {
    val file = new File(filename)
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(text)
    bw.close()
  }

  def main(args: Array[String]): Unit = {
    if (args.length != 1) {
      println("Path to a directory containing CIM files must be provided as an argument")
      System.exit(1)
    }
    val path = args(0)
    println(s"\n\nProcessing directory $path\n\n")
    fromDirectory(path)
  }

}
