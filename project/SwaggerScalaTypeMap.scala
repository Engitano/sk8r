import io.swagger.models._
import io.swagger._
import io.swagger.models.properties._
import scala.meta._
import scala.collection.JavaConverters._

class SwaggerScalaTypeMap(swagger: Swagger) {

  def isType(what: String, thisType: Type): Boolean = {
    thisType match {
      case Type.Apply(name, _) => name.syntax == what
      case _                   => false
    }
  }

  def isOption(thisType: Type): Boolean = isType("Option", thisType)

  def map(p: Property): Type = {
    if (!p.getRequired && !p.isInstanceOf[MapProperty] && !p
          .isInstanceOf[ArrayProperty])
      t"Option[${mapNonOptional(p)}]"
    else
      mapNonOptional(p)
  }

  def mapNonOptional(p: Property): Type = {
    p match {
      case _: StringProperty      => t"String"
      case _: BooleanProperty     => t"Boolean"
      case _: DoubleProperty      => t"Double"
      case _: FloatProperty       => t"Float"
      case _: IntegerProperty     => t"Int"
      case _: LongProperty        => t"Long"
      case _: BaseIntegerProperty => t"Int"
      case m: MapProperty =>
        t"Map[String, ${mapNonOptional(m.getAdditionalProperties)}]"
      case a: ArrayProperty =>
        t"Seq[${mapNonOptional(a.getItems)}]"
      case _: DecimalProperty => t"BigDecimal"
      case r: RefProperty =>
        val targetModel = swagger.getDefinitions.asScala(r.getSimpleRef())
        val tokens      = r.getSimpleRef.split("\\.").toList
        val pkg = tokens
          .dropRight(1)
          .drop(1)
          .foldLeft[Term.Ref](Term.Name(tokens.head)) { (p, n) =>
            Term.Select(p, Term.Name(n))
          }
        val simpleName = r.getSimpleRef().split("\\.").last.capitalize
        if (Option(targetModel.getProperties())
              .map(_.asScala)
              .filter(_.nonEmpty)
              .isDefined) {
          Type.Select(pkg, Type.Name(simpleName))
        } else {
          Type.Select(
            Term.Select(pkg, Term.Name(simpleName)),
            Type.Name("Alias")
          )
        }
      case _: DateProperty     => t"java.time.LocalDate"
      case _: DateTimeProperty => t"java.time.OffsetDateTime"
      case _: UUIDProperty     => t"java.util.UUID"
      case _: BinaryProperty   => t"Array[Byte]"
      case _: FileProperty     => t"Array[Byte]"
      case o: ObjectProperty   => t"io.circe.Json"
      case _: PasswordProperty => t"String"

      case null => throw new Exception("Trying to resolve null property")
      case x    => throw new Exception(s"unexpected property type $x")
    }
  }
}
