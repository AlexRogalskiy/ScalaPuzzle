package wildbeeslabs.classes

import wildbeeslabs.traits.AppType
import wildbeeslabs.classes.Rectangle._
import wildbeeslabs.classes.CMatrix._

class RectangleMask[T >: Null <: Rectangle[Int]] (
	private var leftBottom: T,
	private var leftTop: T,
	private var rightTop: T,
	private var rightBottom: T) extends ShapeMask[RectangleMask[T], String] with AppType {

	def this() 		= this(null, null, null, null)
	def lBottom: T 	= leftBottom
	def lTop: T 	= leftTop
	def rTop: T 	= rightTop
	def rBottom: T 	= rightBottom
	def hasPlaceholder(placeHolder: T = null): Boolean = this.borderSet.contains(getShapeID[RectInt, Int](placeHolder))

	def cValue: Int = (leftTop.rBottom + rightTop.lBottom + rightBottom.lTop + leftBottom.rTop)
	def lValue: Int = (leftTop.lBottom + leftBottom.lTop)
	def tValue: Int = (leftTop.rTop + rightTop.lTop)
	def rValue: Int = (rightTop.rBottom + rightBottom.rTop)
	def bValue: Int = (rightBottom.lBottom + leftBottom.rBottom)

	def intersectLeft(rectangleMask2: RectangleMask[T]): Set[String] 	= this.intersectMask[String](this.leftBorderSet, rectangleMask2.borderSet)
	def intersectRight(rectangleMask2: RectangleMask[T]): Set[String] 	= this.intersectMask[String](this.rightBorderSet, rectangleMask2.borderSet)
	def intersectTop(rectangleMask2: RectangleMask[T]): Set[String] 	= this.intersectMask[String](this.topBorderSet, rectangleMask2.borderSet)
	def intersectBottom(rectangleMask2: RectangleMask[T]): Set[String] 	= this.intersectMask[String](this.bottomBorderSet, rectangleMask2.borderSet)

	def leftBorder: Tuple2[T, T] 		= Tuple2(leftBottom, leftTop)
	def topBorder: Tuple2[T, T] 		= Tuple2(leftTop, rightTop)
	def rightBorder: Tuple2[T, T] 		= Tuple2(rightBottom, rightTop)
	def bottomBorder: Tuple2[T, T]		= Tuple2(leftBottom, rightBottom)
	def border: Tuple4[T, T, T, T]		= Tuple4(leftTop, rightTop, leftBottom, rightBottom)

	val leftBorderSet: Set[String] 		= tuple2ToSet[T](this.leftBorder).map(getShapeID[RectInt, Int](_))
	val topBorderSet: Set[String] 		= tuple2ToSet[T](this.topBorder).map(getShapeID[RectInt, Int](_))
	val rightBorderSet: Set[String] 	= tuple2ToSet[T](this.rightBorder).map(getShapeID[RectInt, Int](_))
	val bottomBorderSet: Set[String] 	= tuple2ToSet[T](this.bottomBorder).map(getShapeID[RectInt, Int](_))
	val borderSet: Set[String] 			= topBorderSet.union(bottomBorderSet)

	override def diff(rectangleMask2: RectangleMask[T], f: (String) => Boolean = (String) => true): Set[String] = {
		return this.borderSet.filter(f(_)).diff(rectangleMask2.borderSet)
	}
	override def intersect(rectangleMask2: RectangleMask[T], f: (String) => Boolean = (String) => true): Set[String] = {
		return this.intersectMask[String](this.borderSet.filter(f(_)), rectangleMask2.borderSet)
	}

	def validate(placeHolder: T): Boolean = {
		val defaultMaxBoundValue: Int = 10
		def isPerimeterValid(): Boolean = {
			return (this.lValue <= defaultMaxBoundValue &&
					this.tValue <= defaultMaxBoundValue &&
					this.rValue <= defaultMaxBoundValue &&
					this.bValue <= defaultMaxBoundValue)
		}
		def isFullValid(): Boolean = {
			if(this.cValue == defaultMaxBoundValue && isPerimeterValid())
				return true
			return false
		}
		def isPartialValid(): Boolean = {
			if(this.cValue <= defaultMaxBoundValue && isPerimeterValid())
				return true
			return false
		}
		if(this.hasPlaceholder(placeHolder))
			return isPartialValid()
		return isFullValid()
	}

	def toMatrix[A >: Int <: Int]: CMatrix[Rectangle[A], A] = {
		var matrix = CMatrix[Rectangle[A], A](topBorderSet.size, leftBorderSet.size)
		matrix.fill(tuple4ToList[T](this.border))
		return matrix
	}

	override def toString = s"\n{ RectangleMask => \nleftTop: ($lTop), \nrightTop: ($rTop), \nleftBottom: ($lBottom), \nrightBottom: ($rBottom) }"
}

object RectangleMask extends AppType {
	implicit def apply(leftBottom: RectInt, leftTop: RectInt, rightTop: RectInt, rightBottom: RectInt) = init(leftBottom, leftTop, rightTop, rightBottom)
	def init(leftBottom: RectInt, leftTop: RectInt, rightTop: RectInt, rightBottom: RectInt): RectangleMask[RectInt] = new RectangleMask[RectInt](leftBottom, leftTop, rightTop, rightBottom)
}