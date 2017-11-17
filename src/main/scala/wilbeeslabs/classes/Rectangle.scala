package wildbeeslabs.classes

import wildbeeslabs.traits.AppType

class Rectangle[T >: Int <: Int] (
	private var leftBottom: T,
	private var leftTop: T,
	private var rightTop: T,
	private var rightBottom: T) extends Shape[Rectangle[T], Int] {

	private var placeHolder: Boolean = false

	def this() 		= this(0, 0, 0, 0)
	def lBottom: T 	= leftBottom
	def lTop: T 	= leftTop
	def rTop: T 	= rightTop
	def rBottom: T 	= rightBottom
	def isPlaceHolder: Boolean = placeHolder
 	def setPlaceHolder(placeHolder: Boolean): Unit = this.placeHolder = placeHolder
	def debug: String = s"\n{ Rectangle => ID: ($ID), leftTop: ($lTop), rightTop: ($rTop), rightBottom: ($rBottom), leftBottom: ($lBottom) }"

 	override def *(rectangle2: Rectangle[T]) 	= multiply(this, rectangle2)
 	override def *(value: Int)					= multiply(this, value)
 	override def +(rectangle2: Rectangle[T]) 	= add(this, rectangle2)
 	override def -(rectangle2: Rectangle[T]) 	= substract(this, rectangle2)
	override def sum: T							= (this.lBottom + this.lTop + this.rTop + this.rBottom)

	override def filter(rectangles: List[Rectangle[T]]) (f: (Rectangle[T]) => Boolean): List[Rectangle[T]] = {
		(for (rectangle <- rectangles; if f(rectangle)) yield rectangle).to[List]
	}

	private def multiply(rectangle2: Rectangle[T]): Unit = {
		this.leftBottom 	*= rectangle2.lBottom
		this.leftTop 		*= rectangle2.lTop
		this.rightTop 		*= rectangle2.rTop
		this.rightBottom 	*= rectangle2.rBottom
	}

	private def multiply(value: Int): Unit = {
		this.leftBottom 	*= value
		this.leftTop 		*= value
		this.rightTop 		*= value
		this.rightBottom 	*= value
	}

	private def add(rectangle2: Rectangle[T]): Unit = {
		this.leftBottom 	+= rectangle2.lBottom
		this.leftTop 		+= rectangle2.lTop
		this.rightTop 		+= rectangle2.rTop
		this.rightBottom 	+= rectangle2.rBottom
	}

 	private def substract(rectangle2: Rectangle[T]): Unit = {
		this.leftBottom 	-= rectangle2.lBottom
		this.leftTop 		-= rectangle2.lTop
		this.rightTop 		-= rectangle2.rTop
		this.rightBottom 	-= rectangle2.rBottom
 	}

	private def multiply(rectangle1: Rectangle[T], rectangle2: Rectangle[T]): Rectangle[T] = {
		new Rectangle[T](rectangle1.lBottom * rectangle2.lBottom,
					  	rectangle1.lTop 	* rectangle2.lTop,
					  	rectangle1.rTop 	* rectangle2.rTop,
					  	rectangle1.rBottom 	* rectangle2.rBottom
		)
	}

	private def multiply(rectangle1: Rectangle[T], value: Int): Rectangle[T] = {
		new Rectangle[T](rectangle1.lBottom * value,
					 	rectangle1.lTop 	* value,
					  	rectangle1.rTop 	* value,
					  	rectangle1.rBottom 	* value
		)
	}

	private def add(rectangle1: Rectangle[T], rectangle2: Rectangle[T]): Rectangle[T] = {
		new Rectangle[T](rectangle1.lBottom + rectangle2.lBottom,
					  	rectangle1.lTop 	+ rectangle2.lTop,
					  	rectangle1.rTop 	+ rectangle2.rTop,
					  	rectangle1.rBottom 	+ rectangle2.rBottom
		)
	}

 	private def substract(rectangle1: Rectangle[T], rectangle2: Rectangle[T]): Rectangle[T] = {
		new Rectangle[T](rectangle1.lBottom - rectangle2.lBottom,
						rectangle1.lTop 	- rectangle2.lTop,
					  	rectangle1.rTop 	- rectangle2.rTop,
					  	rectangle1.rBottom 	- rectangle2.rBottom
		)
 	}

 	override def toStringFormat(delim: String = " "): String = s"$lTop$delim$rTop$delim$lBottom$delim$rBottom"
	override def toString = s"$lTop $rTop $lBottom $rBottom"
}

object Rectangle extends AppType {
	private val defaultPruneRectangle = new Rectangle[Int](0, 0, 0, 0)
	defaultPruneRectangle.setPlaceHolder(true)
	private val defaultEmptyRectangle = new Rectangle[Int](0, 0, 0, 0)

	implicit def apply(leftBottom: Int, leftTop: Int, rightTop: Int, rightBottom: Int) = init(leftBottom, leftTop, rightTop, rightBottom)
	def init(leftBottom: Int, leftTop: Int, rightTop: Int, rightBottom: Int): RectInt = new Rectangle[Int](leftBottom, leftTop, rightTop, rightBottom)
	def pruneRectangle: RectInt = defaultPruneRectangle
	def emptyRectangle: RectInt = defaultEmptyRectangle
	def typeList: List[RectInt] = List(defaultPruneRectangle, defaultEmptyRectangle)
}