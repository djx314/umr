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
      override def map[H](t: R => H)(implicit classTag: ClassTag[H]): ShapedValue[Rep[H], H] = {
        val mappedValue = ShapedValue(baseRep, shape).<>(f = t, g = { (_: H) => Option.empty[R] })
        ShapedValue(mappedValue, RepShape[FlatShapeLevel, Rep[H], H])
      }
    }
  }

  def toJson[U, M, R](key: String, rep: U)(implicit shape: Shape[_ <: FlatShapeLevel, U, M, R], encoder: Encoder[M]): ShapedValue[Rep[(String, Json)], (String, Json)] = {
    repMap(rep)(shape).map { (s: M) => key -> s.asJson(encoder) }
  }

  trait SetHelper[M] {
    def value(value: M): ShapedValue[Rep[Placeholder], Placeholder]
  }

  def set[U, M, R](baseRep: U)(implicit shape: Shape[_ <: FlatShapeLevel, U, M, R]): SetHelper[M] = {
    new SetHelper[M] {
      override def value(t: M): ShapedValue[Rep[Placeholder], Placeholder] = {
        val mappedValue = ShapedValue(baseRep, shape).<>(f = { (_: M) => Placeholder: Placeholder }, g = { (_: Placeholder) => Option(t) })(implicitly[ClassTag[Placeholder]])
        ShapedValue(mappedValue, RepShape[FlatShapeLevel, Rep[Placeholder], Placeholder])
      }
    }
  }

}

object ChangeList extends ShapeHelper {

  def apply(list: ShapedValue[Rep[Placeholder], Placeholder]*): ShapedValue[Rep[Placeholder], Placeholder] = {
    val mappedValue = ShapedValue(
      list.toSeq,
      implicitly[Shape[_ <: FlatShapeLevel, Seq[ShapedValue[Rep[Placeholder], Placeholder]], Seq[Placeholder], Seq[ShapedValue[Rep[Placeholder], Placeholder]]]])
    val newShapeValue = mappedValue.<>(f = { (_: Seq[Placeholder]) => Placeholder: Placeholder }, g = { (_: Placeholder) => Option(List.fill(list.size)(Placeholder: Placeholder)) })
    ShapedValue(newShapeValue, RepShape[FlatShapeLevel, Rep[Placeholder], Placeholder])
  }

}