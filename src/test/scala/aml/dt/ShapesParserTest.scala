package aml.dt

import amf.plugins.document.vocabularies.model.document.Dialect
import org.scalatest.AsyncFunSuite

class ShapesParserTest extends AsyncFunSuite with Utils {

  test("it should parse an AML dialect and generate an AML AMF module") {
    loadDialect("file://src/test/resources/schemas.yaml") map { case dialect: Dialect =>
      val shapesParser = new ShapesParser(dialect)
      val module = shapesParser.generate(dialect.location().get, Some(dialect.id), dialect.usage.option())
      assert(module.declares.length == 2)
    }
  }

}
