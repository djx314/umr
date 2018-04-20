package net.scalax.umr

import slick.lifted.{FlatShapeLevel, MappedProjection, Shape, ShapedValue}

import scala.language.existentials
import scala.language.implicitConversions

trait ShapeValueWrap[F] {
  self =>

  type Data
  type Rep
  type TargetRep
  val rep: Rep
  val shape: Shape[FlatShapeLevel, Rep, Data, TargetRep]
  val dataToList: Data => F
  val dataFromList: F => Option[Data]

  def map[H](t: F => H,
             r: H => Option[F] = (s: H) => Option.empty): ShapeValueWrap[H] = {
    new ShapeValueWrap[H] {
      override type Data = self.Data
      override type Rep = self.Rep
      override type TargetRep = self.TargetRep
      override val rep = self.rep
      override val shape = self.shape
      override val dataToList = { (s: Data) =>
        t(self.dataToList(s))
      }
      override val dataFromList = { (s: H) =>
        r(s).flatMap(t => self.dataFromList(t))
      }
    }
  }
}

object ShapeValueWrap {

  implicit def toWrap[R, D, T](rep: R)(
      implicit shape: Shape[FlatShapeLevel, R, D, T]): ShapeValueWrap[D] = {
    val rep1 = rep
    val shape1 = shape
    new ShapeValueWrap[D] {
      override type TargetRep = T
      override type Data = D
      override type Rep = R
      override val shape = shape1
      override val dataToList = { (data: D) =>
        data
      }
      override val dataFromList = { (data: D) =>
        Option(data)
      }
      override val rep = rep1
    }
  }

}

trait ShapeListWrap[F] {
  type Data
  type Rep
  type TargetRep
  val rep: Rep
  val shape: Shape[FlatShapeLevel, Rep, Data, TargetRep]
  val dataToList: Data => List[F]
  val dataFromList: List[F] => Option[Data]
}

object ListShape {

  def apply[T](v: ShapeValueWrap[T]*): ShapedValue[Any, List[T]] = {
    val sWrap = v.toList match {
      case head :: tail =>
        val initWrap: ShapeListWrap[T] = new ShapeListWrap[T] {
          override type Data = head.Data
          override type Rep = head.Rep
          override type TargetRep = head.TargetRep
          override val rep: Rep = head.rep
          override val shape = {
            head.shape
          }
          override val dataToList: Data => List[T] = data =>
            List(head.dataToList(data))
          override val dataFromList: List[T] => Option[Data] = { data =>
            head.dataFromList(data.head)
          }
        }

        tail.foldLeft(initWrap) { (wrap, current) =>
          val currentWrap = new ShapeListWrap[T] {
            override type Data = (current.Data, wrap.Data)
            override type Rep = (current.Rep, wrap.Rep)
            override type TargetRep = (current.TargetRep, wrap.TargetRep)
            override val rep: Rep = (current.rep, wrap.rep)
            override val shape = {
              Shape.tuple2Shape(current.shape, wrap.shape)
            }
            override val dataFromList
              : List[T] => Option[(current.Data, wrap.Data)] = { data =>
              data match {
                case dataHead :: dataTail =>
                  for {
                    s <- current.dataFromList(dataHead)
                    t <- wrap.dataFromList(dataTail)
                  } yield {
                    (s, t)
                  }
              }
            }
            override val dataToList: ((current.Data, wrap.Data)) => List[T] = {
              data =>
                val (h, t) = data
                current.dataToList(h) :: wrap.dataToList(t)
            }
          }: ShapeListWrap[T]

          currentWrap
        }
      case List(head) =>
        new ShapeListWrap[T] {
          override type Data = head.Data
          override type Rep = head.Rep
          override type TargetRep = head.TargetRep
          override val rep: Rep = head.rep
          override val shape = {
            head.shape
          }
          override val dataToList: Data => List[T] = data =>
            List(head.dataToList(data))
          override val dataFromList: List[T] => Option[Data] = { data =>
            head.dataFromList(data.head)
          }
        }: ShapeListWrap[T]
      case _ =>
        new ShapeListWrap[T] {
          override type Data = Unit
          override type Rep = Unit
          override type TargetRep = Unit
          override val rep: Rep = (())
          override val shape = {
            Shape.unitShape[FlatShapeLevel]
          }
          override val dataFromList: List[T] => Option[Unit] = data =>
            Option(())
          override val dataToList: Unit => List[T] = { data =>
            List.empty
          }
        }: ShapeListWrap[T]
    }

    val tatalShapeValue = ShapedValue(sWrap.rep, sWrap.shape)

    ShapedValue(
      tatalShapeValue
        .<>[List[T]](
          s => sWrap.dataToList(s),
          s => sWrap.dataFromList(s)
        ),
      implicitly[Shape[FlatShapeLevel,
                       MappedProjection[List[T], sWrap.Data],
                       List[T],
                       MappedProjection[List[T], sWrap.Data]]]
    ).asInstanceOf[ShapedValue[Any, List[T]]]
  }

}
