package aml.dt

import amf.plugins.document.vocabularies.model.document.Dialect
import org.scalatest.AsyncFunSuite

class GeneratorTest extends AsyncFunSuite with Utils {
  test("it should render a RAML 1.0 data types library") {
    loadDialect("file://src/test/resources/schemas.yaml") map { case dialect: Dialect =>
      val generated = new Generator(dialect).generate("RAML 1.0")
      val cs = platform.fs.syncFile("src/test/resources/schemas.raml").read()
      val target = cs.subSequence(0, cs.length()).toString
      assert(generated == target)
    }
  }

  test("it should render a JSON-Schema data types library") {
    loadDialect("file://src/test/resources/schemas.yaml") map { case dialect: Dialect =>
      val generated = new Generator(dialect).generate("JSON-Schema")
      val cs = platform.fs.syncFile("src/test/resources/schemas.json").read()
      val target = cs.subSequence(0, cs.length()).toString
      assert(generated == target)
    }
  }
}
