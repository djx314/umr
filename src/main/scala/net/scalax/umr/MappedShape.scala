package net.scalax.umr

import io.circe.{Encoder, Json}
import io.circe.syntax._
import slick.lifted.ShapedValue

object MappedShape {

  def toJson[M](key: String, rep: ShapeValueWrap[M])(
      implicit encoder: Encoder[M]): ShapeValueWrap[(String, Json)] = {
    rep.map[(String, Json)]({ (s: M) =>
      (key, s.asJson(encoder))
    })
  }

  trait SetHelper[M] {
    def value(value: M): ShapeValueWrap[Placeholder]
  }

  def set[U](wrap: ShapeValueWrap[U]): SetHelper[U] = {
    new SetHelper[U] {
      override def value(t: U): ShapeValueWrap[Placeholder] = {
        wrap.map[Placeholder]({ (s: U) =>
          Placeholder: Placeholder
        }, (s: Placeholder) => Option(t))
      }
    }
  }

}

object ChangeList {

  import slick.jdbc.H2Profile.api._

  def apply(
      list: ShapeValueWrap[Placeholder]*): ShapedValue[Any, Placeholder] = {
    val mappedValue = ListShape(list: _*)
    mappedValue
      .<>[Placeholder](
        _ => (Placeholder: Placeholder),
        _ => Option(List.fill(list.size)(Placeholder: Placeholder)))
      .shaped
      .asInstanceOf[ShapedValue[Any, Placeholder]]
  }

}
