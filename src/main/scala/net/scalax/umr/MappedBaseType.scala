package net.scalax.umr

import io.circe.{ Encoder, Json }
import io.circe.syntax._
import slick.jdbc.JdbcProfile
import slick.lifted._

import scala.reflect.ClassTag

object SomeTest {

  def mapWritter[T, U](rep: Rep[U])(tcomap: U => T)(implicit profile: JdbcProfile, classTag: ClassTag[T], colType: JdbcProfile#BaseColumnType[U]): Rep[T] = {
    val detailProfile = profile
    import detailProfile.api._
    implicit val jdbcType = MappedColumnType.base[T, U]({ (_: T) => ??? }, tcomap)
    new AnyExtensionMethods(rep.toNode).asColumnOf[T]
  }

  implicit def repShape[Level <: ShapeLevel]: Shape[Level, Rep[(String, Json)], (String, Json), Rep[(String, Json)]] = {
    RepShape[Level, Rep[(String, Json)], (String, Json)]
  }

  def toJson[U](key: String, rep: Rep[U])(implicit profile: JdbcProfile, colType: JdbcProfile#BaseColumnType[U], encoder: Encoder[U]): Rep[(String, Json)] = {
    implicit val jsonClassTag = implicitly[ClassTag[Json]]
    implicit val jdbcType = profile.MappedColumnType.base[(String, Json), U]({ (_: (String, Json)) => ??? }, { (data: U) => key -> data.asJson })
    new AnyExtensionMethods(rep.toNode).asColumnOf[(String, Json)]
  }

}