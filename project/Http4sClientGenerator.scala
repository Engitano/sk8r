import io.swagger.models.parameters._
import io.swagger.models.Swagger
import io.swagger.models.properties.PropertyBuilder
import io.swagger.models.properties.RefProperty
import scala.collection.JavaConverters._
import scala.meta._

class Http4sClientGenerator(swagger: Swagger) {

  val mapper = new SwaggerScalaTypeMap(swagger)

  private def imports: List[Import] = {
    List(
      q"import org.http4s._",
      q"import org.http4s.headers._",
      q"import org.http4s.MediaType._",
      q"import org.http4s.client._",
      q"import org.http4s.circe.CirceEntityEncoder._",
      q"import org.http4s.circe.CirceEntityDecoder._",
      q"import io.circe.generic.auto._",
      q"import cats.effect._",
      q"import cats.syntax.flatMap._",
      q"import CirceSerdes._"
    )
  }

  def toPackage(defName: String): Term.Ref = {
    val tokens = defName.split("\\.").dropRight(1)
    tokens
      .dropRight(1)
      .drop(1)
      .foldLeft[Term.Ref](Term.Name(tokens.head)) { (p, n) =>
        Term.Select(p, Term.Name(n))
      }
  }

  def generateClient(
      models: Map[String, ScalaSwaggerEntity]
  ): List[Stat] = {
    val funcs = swagger.getPaths.asScala.flatMap {
      case (uri, path) =>
        path.getOperationMap().asScala.filter(!_._2.getOperationId.contains("beta")).toMap.map {
          case (method, operation) =>
            val funcName         = operation.getOperationId
            val uriConstSegments = uri.split("\\{[^\\}]+\\}", -1).toList
            val queryParams = operation
              .getParameters
              .asScala
              .collect {
                case p: QueryParameter =>
                  val prop =
                    PropertyBuilder.build(p.getType, p.getFormat, null)
                  val realTpe = mapper.map(prop)
                  Term.Param(List.empty, Name("q" + p.getName.capitalize), Some(realTpe), if(!p.getRequired) Some(Term.Name("None")) else None)
              }
              .toList
            val pathParams = operation
              .getParameters
              .asScala
              .collect {
                case p: PathParameter =>
                  val prop =
                    PropertyBuilder.build(p.getType, p.getFormat, null)
                  val realTpe = mapper.mapNonOptional(prop)
                  Term.Param(List.empty, Name("p" + p.getName.capitalize), Some(realTpe), None)
              }
              .toList
            val bodyParams = operation
              .getParameters
              .asScala
              .collect {
                case bp: BodyParameter =>
                val arg = new RefProperty(bp.getSchema.getReference)
                  val realTpe =
                    mapper.mapNonOptional(arg)
                  Term.Param(List.empty, Name("payload"), Some(realTpe), None)
              }
              .toList
            // org.http4s.Request(method = Method.GET, uri = Uri.uri(""), headers = Headers(List(Header("","")),body = null))
            val resultType = operation
              .getResponses
              .asScala
              .collectFirst {
                case (c, r) if c.startsWith("20") => mapper.mapNonOptional(r.getSchema)
              }
              .getOrElse(Type.Name("Unit"))


            val headerTerm = q"""Headers(
              List(
                Authorization(Credentials.Token(AuthScheme.Bearer, ${Term.Name("bearerToken")})),
                Accept(application.json)
              ))"""
            val query = q"""Map(..${queryParams.map(
              u => q"${Lit.String(u.name.value)} -> Seq(${Term.Name(u.name.value)}.map(_.toString)).flatten"
            )})"""
            val reqPath = Term.Interpolate(
              Term.Name("s"),
              uriConstSegments.map(u => Lit.String(u)),
              pathParams.map(p => Term.Name(p.name.value)).reverse
            )
            val reqMethod = Term.Select(
              Term.Name("Method"),
              Term.Name(method.toString().toUpperCase())
            )
            // val addBody = if(bodyParams.nonEmpty) q"withBody(${Term.Name(bodyParams.head.name.value)})" else q""
            (
              q"def ${Term.Name(funcName)}(..${pathParams.toList ++ bodyParams.toList ++ queryParams.toList}): F[$resultType]",
              q"""def ${Term
                .Name(funcName)}(..${pathParams.toList ++ bodyParams.toList ++ queryParams.toList}): F[$resultType] = 
                tokenSource.getBearerToken.flatMap { bearerToken =>
                  val headers = $headerTerm
                  val request = Request[F](method = ${reqMethod}, uri = baseUri.withPath(${reqPath}).setQueryParams[String, String](${query}), headers = headers)
                  val requestWithBody = ${if(bodyParams.nonEmpty) q"request.withEntity(${Term.Name(bodyParams.head.name.value)})" else q"request" } 
                  httpClient.expect[${resultType}](requestWithBody)
                }"""
            )
        }
    }
    imports ++
    List(
    q"""trait KubernetesClient[F[_]]{
      ..${funcs.map(_._1).toList}
    }""",
    q"""object KubernetesClient{
      def apply[F[_]: Sync](httpClient: Client[F], baseUri: Uri, tokenSource: TokenSource[F]): KubernetesClient[F] = new KubernetesClient[F] {
        ..${funcs.map(_._2).toList}
      }
    }""")
  }
}
