package aml.dt

import amf.core.annotations.DeclaredElement
import amf.core.model.document.{BaseUnit, Module}
import amf.core.model.domain.Shape
import amf.core.model.domain.extensions.PropertyShape
import amf.plugins.document.vocabularies.model.document.{Dialect, DialectFragment, DialectLibrary}
import amf.plugins.document.vocabularies.model.domain.NodeMapping
import amf.plugins.domain.shapes.models.{ArrayShape, NodeShape, ScalarShape, UnionShape}

import scala.collection.mutable


class ShapesParser(dialectUnit: BaseUnit) {

  protected val nodeMap: mutable.Map[String, NodeShape] = mutable.Map()
  protected var c: Int = 0

  def generate(location: String, id: Option[String], usage: Option[String]): Module = {
    val moduleId = id.getOrElse("http://aml2dt.com/Module")
    val module = Module().withLocation(location).withId(moduleId)
    usage.foreach(module.withUsage)

    val declarations = collectUnits
    declarations.foreach(parseNodeMappingWithoutProperties)
    declarations.foreach(parseNodeMappingProperties)

    module.withDeclares(nodeMap.values.toSeq)
  }

  protected def collectUnits: Seq[NodeMapping] = {
    val domainElements = dialectUnit match {
      case d: Dialect =>
        Option(d.encodes).map(Seq(_)).getOrElse(Nil) ++ d.declares
      case d: DialectLibrary =>
        d.declares
      case d: DialectFragment =>
        Option(d.encodes).map(Seq(_)).getOrElse(Nil)
    }

    domainElements.collect { case x if x.isInstanceOf[NodeMapping] => x.asInstanceOf[NodeMapping] }
  }

  protected def parseNodeMappingWithoutProperties(nodeMapping: NodeMapping): NodeShape = {
    val shape = NodeShape().withId(nodeMapping.id)

    shape.annotations += DeclaredElement()

    nodeMapping.name.option() match {
      case Some(name) => shape.withName(name)
      case _          => // ignore
    }

    nodeMap.put(nodeMapping.id, shape)
    shape
  }

  protected def parseNodeMappingProperties(nodeMapping: NodeMapping): Shape = {
    val shape = nodeMap(nodeMapping.id)

    val propertyShapes = nodeMapping.propertiesMapping() map { propertyMapping =>
      val propertyShape = PropertyShape().withId(propertyMapping.id)
      propertyMapping.name().option() match {
        case Some(name) => propertyShape.withName(name)
        case _          => // ignore
      }
      propertyMapping.minCount().option().foreach(propertyShape.withMinCount)
      propertyMapping.literalRange().option().foreach { dataType =>
        val scalar = ScalarShape().withDataType(dataType)
        propertyShape.withRange(scalar)
      }

      val objectRangeIds = propertyMapping.objectRange().collect { case range if range.option().isDefined => range.value() }
      val objectRanges = objectRangeIds.map { id =>
        val targetShape = nodeMap(id)
        targetShape.link(targetShape.name.value()).asInstanceOf[Shape]
      }

      val objRange = if (objectRanges.length == 1) {
        Some(objectRanges.head)
      } else if (objectRanges.length > 1) {
        c += 1
        Some(UnionShape().withId(s"http://aml2dt.com/autogen/${c}").withName(s"Union${c}").withAnyOf(objectRanges))
      } else None

      objRange.foreach { shape =>
        if (propertyMapping.allowMultiple().option().getOrElse(false)) {
          c += 1
          val array = ArrayShape().withId(s"http://aml2dt.com/autogen${c}").withName(s"Array${shape.name.value()}").withItems(shape)
          propertyShape.withRange(array)
        } else {
          propertyShape.withRange(shape)
        }
      }

      propertyShape
    }

    shape.withProperties(propertyShapes)
  }


}
