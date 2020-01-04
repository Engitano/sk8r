import io.swagger.models.ModelImpl
import io.swagger.parser.processors.PropertyProcessor
import io.swagger.models.properties.PropertyBuilder.PropertyId
import io.swagger.models.Model
import io.swagger.parser.SwaggerParser
import io.swagger.models.properties._
import io.swagger.models.Swagger
import scala.meta._
import scala.collection.JavaConverters._
import cats.implicits._
import cats.instances.MapInstances
import cats.Later

// case class CaseClassProperty(name: String, `type`: String, comment: Option[String])
// case class CaseClass(name: String, fields: List[CaseClassProperty], comment: Option[String])

sealed trait ScalaSwaggerEntity {
  def stat: Stat = this match {
    case CaseClass(name, fields) =>
      q"case class ${name} (..${fields.value})"
    case TypeAlias(name, aliasFor) =>
      q"package object ${Term.Name(name.value)} { type Alias = $aliasFor }"
  }
}
case class CaseClass(name: Type.Name, fields: Later[List[Term.Param]])
    extends ScalaSwaggerEntity
case class TypeAlias(name: Type.Name, aliasFor: Type) extends ScalaSwaggerEntity

class ModelGenerator(swagger: Swagger) {

  val mapper = new SwaggerScalaTypeMap(swagger)

  def propertiesToParams(
      defs: Map[String, Model]
  )(
      properties: Map[String, Property]
  ): List[Term.Param] = {
    properties.toList.sortBy(!_._2.getRequired).map {
      case (pname, prop) =>
        val default = if (prop.getRequired) None else Some(Term.Name("None"))
        Term.Param(
          List.empty,
          Name(pname),
          Some(mapper.map(prop)),
          default
        )
    }
  }

  def generateStatement(
      models: Map[String, Model]
  )(
      name: String,
      model: Model
  ): ScalaSwaggerEntity =
    Option(model.getProperties()).map(_.asScala).filter(_.nonEmpty) match {
      case None =>
        val modelType = model match {
          case m: ModelImpl => Option(m.getType())
          case _            => None
        }
        val tp = modelType
          .map(t => PropertyBuilder.build(t, null, null))
          .map(mapper.map)
          .getOrElse(t"io.circe.Json")

        TypeAlias(Type.Name(name), tp)
      // q"type ${Type.Name(name)} = $nomdeplume"

      case Some(parameters) =>
        val props = Option(model.getProperties)
          .map(_.asScala.toList)
          .filter(_.nonEmpty)
          .map(p => Later(propertiesToParams(models)(p.toMap)))
          .getOrElse(Later(List[Term.Param]()))
        CaseClass(Type.Name(name), props)
      // q"case class ${Type.Name(name)} (..${parameters.value})")
    }

  def generateEntity(
      models: Map[String, Model]
  )(
      pair: (String, Model)
  ): (String, ScalaSwaggerEntity) = {
    val (name, model) = pair
    val className = name.split("\\.").last.capitalize
    name -> generateStatement(models)(className, model)
  }

  def generate(): List[(String, ScalaSwaggerEntity)] = {
    val defs = swagger.getDefinitions.asScala.toMap
    val res = defs.toList
      .foldLeft[Map[String, ScalaSwaggerEntity]](Map()) { (p, n) =>
        val ents = generateEntity(defs)(n)
        p + ents
      }
    res.toList
  }

}
