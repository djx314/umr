package net.scalax.umr

import slick.jdbc.JdbcProfile
import slick.lifted.{ AnyExtensionMethods, Rep }

import scala.reflect.ClassTag

object SomeTest {

  def mapWritter[T, U](rep: Rep[U], tcomap: U => T)(implicit profile: JdbcProfile, classTag: ClassTag[T], colType: JdbcProfile#BaseColumnType[U]): Rep[T] = {
    val detailProfile = profile
    import detailProfile.api._
    implicit val jdbcType = MappedColumnType.base({ s => ??? }, tcomap)
    new AnyExtensionMethods(rep.toNode).asColumnOf[T]
  }

}