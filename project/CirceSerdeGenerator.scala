import scala.meta._

object ClientSerdeGenerator {

    private def toPackage(defName: String): Term.Ref = {
        val tokens = defName.split("\\.").dropRight(1)
        tokens
          .drop(1)
          .foldLeft[Term.Ref](Term.Name(tokens.head)) { (p, n) =>
            Term.Select(p, Term.Name(n))
          }
      }

    private def imports = List(
        q"import io.circe._",
        q"import io.circe.generic.semiauto._"
    )

    def generateImplicits(code: Map[String, ScalaSwaggerEntity]): List[Stat] = {
        code.toList.collect {
            case (s, c: CaseClass) => s -> c 
        }.flatMap { t => 
            List(
                q"implicit def ${Term.Name(t._1.replace(".","") + t._2.name.value.toLowerCase() + "Decoder")}: Decoder[${toPackage(t._1)}.${t._2.name}] = deriveDecoder",
                q"implicit def ${Term.Name(t._1.replace(".","") + t._2.name.value.toLowerCase() + "Encoder")}: Encoder[${toPackage(t._1)}.${t._2.name}] = deriveEncoder"
            )
        }
    }

    def generate(code: Map[String, ScalaSwaggerEntity]): List[Stat] = {
        imports ++ List(q"object CirceSerdes { ..${generateImplicits(code)} }")
    }
}