package wildbeeslabs.classes

import wildbeeslabs.traits.AppType
import wildbeeslabs.classes.Rectangle._
import wildbeeslabs.classes.RectangleMask._
import wildbeeslabs.classes.CMatrix._

class Rectangle2Mask[T >: Null <: RectangleMask[Rectangle[Int]]] (
	private var leftBottom: T,
	private var leftTop: T,
	private var rightTop: T,
	private var rightBottom: T) extends ShapeMask[Rectangle2Mask[T], String] with AppType {

	def this() 					= this(null, null, null, null)
	def left: RectIntMask 		= RectangleMask(leftBottom.lTop, leftBottom.rTop, leftTop.lBottom, leftTop.rBottom)
	def top: RectIntMask 		= RectangleMask(leftTop.rBottom, leftTop.rTop, rightTop.lTop, rightTop.lBottom)
	def right: RectIntMask 		= RectangleMask(rightBottom.lTop, rightTop.lBottom, rightTop.rBottom, rightBottom.rTop)
	def bottom: RectIntMask 	= RectangleMask(leftBottom.rBottom, leftBottom.rTop, rightBottom.lTop, rightBottom.lBottom)
	def center: RectIntMask 	= RectangleMask(leftBottom.rTop, leftTop.rBottom, rightTop.lBottom, rightBottom.lTop)

	def intersectLeft(rectangle2Mask2: Rectangle2Mask[T]): Set[String] 		= this.intersectMask[String](this.leftBorderSet, rectangle2Mask2.borderSet)
	def intersectRight(rectangle2Mask2: Rectangle2Mask[T]): Set[String] 	= this.intersectMask[String](this.rightBorderSet, rectangle2Mask2.borderSet)
	def intersectTop(rectangle2Mask2: Rectangle2Mask[T]): Set[String]	 	= this.intersectMask[String](this.topBorderSet, rectangle2Mask2.borderSet)
	def intersectBottom(rectangle2Mask2: Rectangle2Mask[T]): Set[String]	= this.intersectMask[String](this.bottomBorderSet, rectangle2Mask2.borderSet)

	def leftBorder: RectTupleOf4	= Tuple4(leftBottom.leftBorder._1, leftBottom.leftBorder._2, leftTop.leftBorder._1, leftTop.leftBorder._2)
	def topBorder: RectTupleOf4		= Tuple4(leftTop.topBorder._1, leftTop.topBorder._2, rightTop.topBorder._1, rightTop.topBorder._2)
	def rightBorder: RectTupleOf4 	= Tuple4(rightBottom.rightBorder._1, rightBottom.rightBorder._2, rightTop.rightBorder._1, rightTop.rightBorder._2)
	def bottomBorder: RectTupleOf4	= Tuple4(rightBottom.bottomBorder._1, rightBottom.bottomBorder._2, leftBottom.bottomBorder._1, leftBottom.bottomBorder._2)
	def centerBorder: RectTupleOf4	= Tuple4(leftBottom.rTop, leftTop.rBottom, rightTop.lBottom, rightBottom.lTop)

	val leftBorderSet: Set[String] 		= tuple4ToSet[RectInt](this.leftBorder).map(getShapeID[RectInt, Int](_))
	val topBorderSet: Set[String] 		= tuple4ToSet[RectInt](this.topBorder).map(getShapeID[RectInt, Int](_))
	val rightBorderSet: Set[String] 	= tuple4ToSet[RectInt](this.rightBorder).map(getShapeID[RectInt, Int](_))
	val bottomBorderSet: Set[String]	= tuple4ToSet[RectInt](this.bottomBorder).map(getShapeID[RectInt, Int](_))

	val centerSet: Set[String]			= tuple4ToSet[RectInt](this.centerBorder).map(getShapeID[RectInt, Int](_))
	val borderSet: Set[String]			= topBorderSet.union(bottomBorderSet).union(leftBorderSet).union(rightBorderSet)

	override def diff(rectangle2Mask2: Rectangle2Mask[T], f: (String) => Boolean = (String) => true): Set[String] = {
		return this.borderSet.filter(f(_)).diff(rectangle2Mask2.borderSet)
	}
	override def intersect(rectangle2Mask2: Rectangle2Mask[T], f: (String) => Boolean = (String) => true): Set[String] = {
		return this.intersectMask[String](this.borderSet.filter(f(_)), rectangle2Mask2.borderSet)
	}

	def validate(placeHolder: RectInt = null): Boolean = {
		if(center.validate(placeHolder) &&
			left.validate(placeHolder) &&
			top.validate(placeHolder) &&
			right.validate(placeHolder) &&
			bottom.validate(placeHolder))
			return true
		return false
	}

	def toMatrix[A >: Int <: Int]: CMatrix[Rectangle[A], A] = {
		var matrix = CMatrix[Rectangle[A], A](leftTop.topBorderSet.size + rightTop.topBorderSet.size, leftTop.leftBorderSet.size + leftBottom.leftBorderSet.size)
		matrix.fill(tuple2ToList(leftTop.topBorder) ::: tuple2ToList(rightTop.topBorder) ::: tuple2ToList(leftTop.bottomBorder) ::: tuple2ToList(rightTop.bottomBorder) ::: 
			tuple2ToList(leftBottom.topBorder) ::: tuple2ToList(rightBottom.topBorder) ::: tuple2ToList(leftBottom.bottomBorder) ::: tuple2ToList(rightBottom.bottomBorder))
		return matrix
	}

	override def toString = s"\n{ Rectangle2Mask => \n\tleftTop: ($leftTop), \n\trightTop: ($rightTop), \n\tleftBottom: ($leftBottom), \n\trightBottom: ($rightBottom) }"
}

object Rectangle2Mask extends AppType {
	implicit def apply(leftBottom: RectIntMask, leftTop: RectIntMask, rightTop: RectIntMask, rightBottom: RectIntMask) = init(leftBottom, leftTop, rightTop, rightBottom)
	def init(leftBottom: RectIntMask, leftTop: RectIntMask, rightTop: RectIntMask, rightBottom: RectIntMask): Rectangle2Mask[RectIntMask] = new Rectangle2Mask[RectIntMask](leftBottom, leftTop, rightTop, rightBottom)
}