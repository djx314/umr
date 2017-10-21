package net.scalax.umr

import slick.SlickException
import slick.ast._
import slick.lifted._
import slick.util.{ ConstArray, ProductWrapper, TupleSupport }

import scala.language.higherKinds
import scala.reflect.ClassTag

trait ShapeHelper {
  implicit def seqShapeGen[Level <: FlatShapeLevel, M, U, P](implicit shapes: Shape[_ <: Level, M, U, P]): Shape[Level, Seq[M], Seq[U], Seq[P]] = {
    new ListAnyShape1111(shapes)
  }
}

abstract class ProductNodeShape1111[Level <: ShapeLevel, C, M <: C, U <: C, P <: C] extends Shape[Level, Seq[M], Seq[U], Seq[P]] {
  /** The Shapes for the product elements. */
  val elementShape: Shape[_ <: ShapeLevel, _, _, _]

  /** Build a record value represented by this Shape from its element values. */
  def buildValue(elems: IndexedSeq[Any]): Any

  /**
   * Create a copy of this Shape with new element Shapes. This is used for
   * packing Shapes recursively.
   */
  def copy(shapes: Shape[_ <: ShapeLevel, _, _, _]): Shape[Level, _, _, _]

  /** Get the element value from a record value at the specified index. */
  def getElement(value: Seq[C], idx: Int): Any

  /**
   * Get an Iterator of a record value's element values. The default
   * implementation repeatedly calls `getElement`.
   */
  def getIterator(value: Seq[C]): Iterator[Any] = {
    value.iterator
  }

  def pack(value: Mixed) = {
    val elems = getIterator(value).map { case f => elementShape.pack(f.asInstanceOf[elementShape.Mixed]) }
    buildValue(elems.toIndexedSeq).asInstanceOf[Packed]
  }
  def packedShape: Shape[Level, Packed, Unpacked, Packed] =
    copy(elementShape.packedShape).asInstanceOf[Shape[Level, Packed, Unpacked, Packed]]

  def buildParams(extract: Any => Unpacked): Packed = {
    throw new SlickException("Shape does not have the same Mixed and Unpacked type")
  }
  def encodeRef(value: Mixed, path: Node): Any = {
    val elems = getIterator(value).zipWithIndex.map {
      case (x, pos) => elementShape.encodeRef(x.asInstanceOf[elementShape.Mixed], Select(path, ElementSymbol(pos + 1)))
    }
    buildValue(elems.toIndexedSeq)
  }
  def toNode(value: Mixed): Node = ProductNode(ConstArray.from(getIterator(value).map(value => elementShape -> value).map {
    case (p, f) => p.toNode(f.asInstanceOf[p.Mixed])
  }.toIterable))
}

trait MappedProductShape1111[Level <: ShapeLevel, C, M <: C, U <: C, P <: C] extends ProductNodeShape1111[Level, C, M, U, P] {
  override def toNode(value: Mixed) = TypeMapping(super.toNode(value), MappedScalaType.Mapper(toBase, toMapped, None), classTag)
  def toBase(v: Any) = new ProductWrapper(getIterator(v.asInstanceOf[Seq[C]]).toIndexedSeq)
  def toMapped(v: Any) = buildValue(TupleSupport.buildIndexedSeq(v.asInstanceOf[Product]))
  def classTag: ClassTag[Seq[U]]
}

final class ListAnyShape1111[Level <: ShapeLevel, C, M <: C, U <: C, P <: C](val inputShapes: Shape[_ <: ShapeLevel, M, U, P])
  extends MappedProductShape1111[Level, C, M, U, P] {
  override val elementShape: Shape[_ <: ShapeLevel, _, _, _] = inputShapes
  //override def getIterator(value: Seq[C]): Iterator[Any] = value.toIterator
  override def getElement(value: Seq[C], idx: Int): Any = value(idx)
  override def buildValue(elems: IndexedSeq[Any]): Any = elems
  override def copy(shape: Shape[_ <: ShapeLevel, _, _, _]): Shape[Level, _, _, _] = new ListAnyShape1111(shape)
  override val classTag: ClassTag[Seq[U]] = implicitly[ClassTag[Seq[U]]]
}