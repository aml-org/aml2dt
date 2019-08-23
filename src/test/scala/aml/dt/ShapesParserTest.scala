package aml.dt

import amf.client.AMF
import amf.core.parser.UnspecifiedReference
import amf.core.remote.{Cache, Context}
import amf.core.services.RuntimeCompiler
import amf.core.unsafe.PlatformSecrets
import org.scalatest.AsyncFunSuite

class ShapesParserTest extends AsyncFunSuite with PlatformSecrets {

  test("it should parse an AML dialect and generate an AML AMF module") {
    AMF.init().get()

    RuntimeCompiler("file://src/test/resources/schemas.yaml",
      Some("application/yaml"),
      Some("AML 1.0"),
      Context(platform),
      UnspecifiedReference,
      Cache()) map { dialect =>

      val shapesParser = new ShapesParser(dialect)
      val module = shapesParser.generate(dialect.location().get, Some(dialect.id), dialect.usage.option())
      assert(module.declares.length == 2)
    }
  }

}
