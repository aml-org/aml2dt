package aml.dt

import amf.client.model.document.Module
import amf.client.render.{Oas20Renderer, Raml10Renderer, Renderer}
import amf.core.emitter.JsonSchemaLibraryPlugin
import amf.core.registries.AMFPluginsRegistry
import amf.plugins.document.vocabularies.model.document.Dialect
import amf.plugins.document.webapi.JsonSchemaPlugin
import amf.plugins.domain.shapes.models.UnionShape

class Generator(dialect: Dialect) extends Renderer("JSON-Schema Library", "application/library+schema+json") {
  AMFPluginsRegistry.registerDocumentPlugin(JsonSchemaLibraryPlugin)

  def generate(format: String): String = {
    val module = new ShapesParser(dialect).generate(dialect.location().get, Some(dialect.id), dialect.usage.option())
    format match {
      case "RAML 1.0" =>
        new Raml10Renderer().generateString(Module(module)).get
      case "JSON-Schema" =>
        generateString(Module(module)).get
      case _             =>
        throw new Exception(s"Unknown format ${format}")
    }
  }

}
