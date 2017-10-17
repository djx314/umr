package net.scalax.umr

import slick.lifted.{ MappedProductShape, Shape, ShapeLevel }

import scala.language.higherKinds
import scala.reflect.ClassTag

class SeqShape[Level <: ShapeLevel, C, M <: C, U <: C, P <: C](inputShapes: Seq[Shape[Level, M, U, P]])
  extends MappedProductShape[Level, Seq[C], Seq[M], Seq[U], Seq[P]] {
  override val shapes: Seq[Shape[_ <: ShapeLevel, _, _, _]] = inputShapes
  override def getIterator(value: Seq[C]): Iterator[Any] = value.toIterator
  override def getElement(value: Seq[C], idx: Int): Any = value(idx)
  override def buildValue(elems: IndexedSeq[Any]): Any = elems
  override def copy(shapes: Seq[Shape[_ <: ShapeLevel, _, _, _]]): Shape[Level, _, _, _] = new SeqShape(shapes.map(_.asInstanceOf[Shape[Level, M, U, P]]))
  override val classTag: ClassTag[Seq[U]] = implicitly[ClassTag[Seq[U]]]
}

trait ShapeHelper {

  implicit def seqShapeGen[Level <: ShapeLevel, M, U, P](implicit shapes: Shape[Level, M, U, P]): Shape[Level, Seq[M], Seq[U], Seq[P]] = {
    new SeqShape(shapes)
  }

}