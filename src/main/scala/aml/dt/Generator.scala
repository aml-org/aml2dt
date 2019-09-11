package aml.dt

import amf.client.model.document.Module
import amf.client.render.{Raml10Renderer, Renderer}
import amf.core.emitter.JsonSchemaLibraryPlugin
import amf.core.metamodel.domain.ShapeModel
import amf.core.registries.AMFPluginsRegistry
import amf.plugins.document.vocabularies.model.document.Dialect
import amf.plugins.domain.shapes.models.NodeShape

class Generator(dialect: Dialect) extends Renderer("JSON-Schema Library", "application/library+schema+json") {
  AMFPluginsRegistry.registerDocumentPlugin(JsonSchemaLibraryPlugin)

  def generate(format: String): String = {
    val parser = new ShapesParser(dialect)
    val module = parser.generate(dialect.location().get, Some(dialect.id), dialect.usage.option())
    format match {
      case "RAML 1.0" =>
        parser.generateRamlForeignLinks()
        new Raml10Renderer().generateString(Module(module)).get
      case "JSON-Schema" =>
        parser.generateJsonSchemaLinks()
        procesJsonSchemaInheritance(module)
        try {
          generateString(Module(module)).get
        } catch {
          case e: Exception =>
            println("EXCEPTION!!")
            throw e
        }
      case _             =>
        throw new Exception(s"Unknown format ${format}")
    }
  }

  /**
   * Transforms inheritance in RAML into JSON Schema allOf contraints
   */
  protected def procesJsonSchemaInheritance(module: amf.core.model.document.Module) = {
    val newDeclarations = module.declares.map { case shape: NodeShape =>
        shape.inherits match {
          case Seq(base) =>
            shape.fields.removeField(ShapeModel.Inherits)
            val newBase = NodeShape().withId(shape.id).withName(shape.name.value())
            shape.withId(shape.id + "/child")
            newBase.setArray(ShapeModel.And, Seq(base, shape))
            newBase
          case _         =>
            shape
        }
    }
    module.withDeclares(newDeclarations)
  }

}
