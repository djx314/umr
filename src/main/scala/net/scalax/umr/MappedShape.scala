package net.scalax.umr

import io.circe.{ Encoder, Json }
import io.circe.syntax._
import slick.lifted._

import scala.reflect.ClassTag

object MappedShape {

  trait MapHelper[R] {
    def map[T](t: R => T)(implicit classTag: ClassTag[T]): ShapedValue[Rep[T], T]
  }

  def repMap[T, R](baseRep: T)(implicit shape: Shape[_ <: FlatShapeLevel, T, R, _]): MapHelper[R] = {
    new MapHelper[R] {
      def map[H](t: R => H)(implicit classTag: ClassTag[H]): ShapedValue[Rep[H], H] = {
        val mappedValue = ShapedValue(baseRep, shape).<>(f = t, g = { (_: H) => Option.empty[R] })
        ShapedValue(mappedValue, RepShape[FlatShapeLevel, Rep[H], H])
      }
    }
  }

  def toJson[U, M, R](key: String, rep: U)(implicit shape: Shape[_ <: FlatShapeLevel, U, M, R], encoder: Encoder[M]): ShapedValue[Rep[(String, Json)], (String, Json)] = {
    repMap(rep)(shape).map { (s: M) => key -> s.asJson(encoder) }
  }

}