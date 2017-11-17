package wildbeeslabs.classes

import wildbeeslabs.traits.AppType
import wildbeeslabs.classes.Rectangle._
import wildbeeslabs.classes.RectangleMask._
import wildbeeslabs.classes.CMatrix._

class Rectangle4Mask[T >: Null <: RectangleMask[Rectangle[Int]]] (
	private var left: T,
	private var top: T,
	private var right: T,
	private var bottom: T) extends ShapeMask[Rectangle4Mask[T], String] with AppType {

	def this() 					= this(null, null, null, null)
	def lTop: RectIntMask 		= RectangleMask(left.lTop, Rectangle.pruneRectangle, top.lTop, left.rTop)
	def rTop: RectIntMask 		= RectangleMask(right.lTop, top.rTop, Rectangle.pruneRectangle, right.rTop)
	def lBottom: RectIntMask 	= RectangleMask(Rectangle.pruneRectangle, left.lBottom, left.rBottom, bottom.lBottom)
	def rBottom: RectIntMask 	= RectangleMask(bottom.rBottom, right.lBottom, right.rBottom, Rectangle.pruneRectangle)
	def center: RectIntMask 	= RectangleMask(left.rBottom, left.rTop, right.lTop, right.lBottom)

	def intersectLeft(rectangle2Mask2: Rectangle4Mask[T]): Set[String] 		= this.intersectMask[String](this.leftBorderSet, rectangle2Mask2.borderSet)
	def intersectRight(rectangle2Mask2: Rectangle4Mask[T]): Set[String] 	= this.intersectMask[String](this.rightBorderSet, rectangle2Mask2.borderSet)
	def intersectTop(rectangle2Mask2: Rectangle4Mask[T]): Set[String]	 	= this.intersectMask[String](this.topBorderSet, rectangle2Mask2.borderSet)
	def intersectBottom(rectangle2Mask2: Rectangle4Mask[T]): Set[String]	= this.intersectMask[String](this.bottomBorderSet, rectangle2Mask2.borderSet)

	def leftBorder: RectTupleOf2 	= left.leftBorder
	def topBorder: RectTupleOf2		= top.topBorder
	def rightBorder: RectTupleOf2 	= right.rightBorder
	def bottomBorder: RectTupleOf2	= bottom.bottomBorder
	def centerBorder: RectTupleOf4 	= Tuple4(left.rBottom, left.rTop, right.lTop, right.lBottom)

	def leftTopBorder: RectTupleOf3 	= Tuple3(left.lTop, left.rTop, top.lTop)
	def rightTopBorder: RectTupleOf3 	= Tuple3(right.lTop, right.rTop, top.rTop)
	def leftBottomBorder: RectTupleOf3 	= Tuple3(left.lBottom, left.rBottom, bottom.lBottom)
	def rightBottomBorder: RectTupleOf3 = Tuple3(right.lBottom, right.rBottom, bottom.rBottom)

	val leftBorderSet: Set[String] 		= tuple2ToSet[RectInt](this.leftBorder).map(getShapeID[RectInt, Int](_))
	val topBorderSet: Set[String] 		= tuple2ToSet[RectInt](this.topBorder).map(getShapeID[RectInt, Int](_))
	val rightBorderSet: Set[String] 	= tuple2ToSet[RectInt](this.rightBorder).map(getShapeID[RectInt, Int](_))
	val bottomBorderSet: Set[String]	= tuple2ToSet[RectInt](this.bottomBorder).map(getShapeID[RectInt, Int](_))

	val leftTopBorderSet: Set[String]		= tuple3ToSet[RectInt](this.leftTopBorder).map(getShapeID[RectInt, Int](_))
	val rightTopBorderSet: Set[String]		= tuple3ToSet[RectInt](this.rightTopBorder).map(getShapeID[RectInt, Int](_))
	val leftBottomBorderSet: Set[String]	= tuple3ToSet[RectInt](this.leftBottomBorder).map(getShapeID[RectInt, Int](_))
	val rightBottomBorderSet: Set[String]	= tuple3ToSet[RectInt](this.rightBottomBorder).map(getShapeID[RectInt, Int](_))

	val centerSet: Set[String]			= tuple4ToSet[RectInt](this.centerBorder).map(getShapeID[RectInt, Int](_))
	val borderSet: Set[String]			= leftTopBorderSet.union(rightTopBorderSet).union(leftBottomBorderSet).union(rightBottomBorderSet)

	override def diff(rectangle2Mask2: Rectangle4Mask[T], f: (String) => Boolean = (String) => true): Set[String] = {
		return this.borderSet.filter(f(_)).diff(rectangle2Mask2.borderSet)
	}
	override def intersect(rectangle2Mask2: Rectangle4Mask[T], f: (String) => Boolean = (String) => true): Set[String] = {
		return this.intersectMask[String](this.borderSet.filter(f(_)), rectangle2Mask2.borderSet)
	}

	def validate(placeHolder: RectInt = null): Boolean = {
		if(center.validate(placeHolder) &&
			lTop.validate(placeHolder) &&
			rTop.validate(placeHolder) &&
			rBottom.validate(placeHolder) &&
			lBottom.validate(placeHolder))
			return true
		return false
	}

	def toMatrix[A >: Int <: Int]: CMatrix[Rectangle[A], A] = {
		var matrix = CMatrix[Rectangle[A], A](left.topBorderSet.size + right.topBorderSet.size, top.leftBorderSet.size + bottom.leftBorderSet.size)
		matrix.fill(tuple2ToList(lTop.topBorder) ::: tuple2ToList(rTop.topBorder) ::: tuple2ToList(lTop.bottomBorder) ::: tuple2ToList(rTop.bottomBorder) ::: 
			tuple2ToList(lBottom.topBorder) ::: tuple2ToList(rBottom.topBorder) ::: tuple2ToList(lBottom.bottomBorder) ::: tuple2ToList(rBottom.bottomBorder))
		return matrix
	}

	override def toString = s"\n{ Rectangle4Mask => \n\tleft: ($left), \n\ttop: ($top), \n\tright: ($right), \n\tbottom: ($bottom) }"
}

object Rectangle4Mask extends AppType {
	implicit def apply(left: RectIntMask, top: RectIntMask, right: RectIntMask, bottom: RectIntMask) = init(left, top, right, bottom)
	def init(left: RectIntMask, top: RectIntMask, right: RectIntMask, bottom: RectIntMask): Rectangle4Mask[RectIntMask] = new Rectangle4Mask[RectIntMask](left, top, right, bottom)
}