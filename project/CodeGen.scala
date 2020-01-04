import io.swagger.parser.SwaggerParser
import scala.io.Source
import java.io.PrintWriter
import scala.meta._
import sbt.File
import sbt._
import java.io.File

object CodeGen {

  def generate(pathPrefix: File, apiFile: File): Seq[File] = {

    val json      = Source.fromFile(apiFile).getLines.mkString
    val swagger   = new SwaggerParser().parse(json)
    val models    = new ModelGenerator(swagger).generate()
    val clientGen = new Http4sClientGenerator(swagger)
    val client    = clientGen.generateClient(models.toMap)

    val prefix = pathPrefix

    val files = models.groupBy(_._1).map {
      case (pcg, classes) =>
        val classTokens = pcg.split("\\.")
        val dir        = prefix / classTokens.dropRight(1).mkString("/")
        if (!dir.exists()) {
          dir.mkdirs()
        }
        val file = dir / s"${classTokens.last}.scala"
        val writer = new PrintWriter(file)
        val packageTokens = pcg.split("\\.")
        val packageTerm = packageTokens
          .drop(1)
          .dropRight(1)
          .toList
          .foldLeft[Term.Ref](Term.Name(packageTokens.head)) { (p, n) =>
            Term.Select(p, Term.Name(n))
          }

        writer
          .write(q"package $packageTerm { ..${classes.map(_._2.stat)} }".syntax)
        //   writer.write(classes.map(_.toString).mkString("\n\n"))
        writer.close()
      file
    }.toList
    val clientDir = prefix / "com/engitano/sk8r"
    if (!clientDir.exists()) {
      clientDir.mkdirs()
    }
    val circeData = ClientSerdeGenerator.generate(models.toMap).toList

    val serdeFile = clientDir / "Circe.scala"
    val serdeWriter = new PrintWriter(serdeFile)

    serdeWriter.write(
      q"package com.engitano.sk8r { ..$circeData }".syntax
    )
    //   writer.write(classes.map(_.toString).mkString("\n\n"))
    serdeWriter.close()
    val clientFile = clientDir / "KubernetesClient.scala"
    val writer = new PrintWriter(clientFile)

    writer.write(q"package com.engitano.sk8r { ..$client }".syntax)
    //   writer.write(classes.map(_.toString).mkString("\n\n"))
    writer.close()

    files ++ List(serdeFile, clientFile)
  }
}
