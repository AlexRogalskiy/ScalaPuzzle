package com.wildbeeslabs

object Puzzle extends AppInitializer {
	/* Default program usage information */
	val usage =
	"""
	>>> Program options information <<<

		Usage: run --file filename

	>>> Copyright by wildbeeslabs (C) <<<
	"""
	def main(args: Array[String]): Unit = {
    	if (args.length == 0) {
	        println(usage)
	        return//sys.exit(0)
	    }
    	val options = nextOption(Map().withDefaultValue("Not found"), args.to[List])
    	runPuzzleTask(options)
    }
}

trait AppType {
	type OptionMap 		= Map[Symbol, Any]
	type RectInt 		= Rectangle[Int]
	type RectIntMask 	= RectangleMask[RectInt]
	type RectMaskInt = RectangleMask[Rectangle[Int]]
	type Rect4IntMask 	= Rectangle4Mask[RectIntMask]
}

trait AppInitializer extends AppType {
	import scala.io.Codec
	import scala.io.Source
	import scala.io.BufferedSource
	import scala.collection.mutable.ListBuffer

	implicit val decoder = Codec.UTF8.decoder
	decoder.onMalformedInput(java.nio.charset.CodingErrorAction.IGNORE)
	decoder.onUnmappableCharacter(java.nio.charset.CodingErrorAction.IGNORE)

	private val defaultFileRecordFormat = "\\d{1}\\s+\\d\\s+\\d\\s+\\d"
	private val defaultFileName = "default.txt"

	def runPuzzleTask(map: OptionMap): Unit = {
		def createRectangleMatrix(): CMatrix[RectInt, Int] = {
			var inputDataList = loadDataFromFile[List[Int]] (
								map.getOrElse('file, defaultFileName).toString,
								(str: String) => { str.trim.matches(defaultFileRecordFormat) },
								(str: String) => { str.split("\\s+").map((value) => toInt(value.trim).getOrElse(0)).toList }
			)
			var rectangleList= inputDataList.map(elem => Rectangle(elem(2), elem(0), elem(1), elem(3)))
			val matrixDimension = getMaxPowerOf2(rectangleList.length)
			var resultSet = CMatrix[RectInt, Int](matrixDimension, matrixDimension)
			resultSet.fill(rectangleList, Rectangle.defaultPlaceHolder/*null*/)
			return resultSet
		}
		def generateRectangleCombinations(matrix: CMatrix[RectInt, Int]): Iterable[RectIntMask] = {
			var resultSet = new ListBuffer[RectIntMask]()
			var rectangleCombinations = matrix.combinate(4, (r: List[RectInt]) => true, (r: RectInt) => (r != Rectangle.defaultPlaceHolder)/*(null != r) // (r != Rectangle.defaultPlaceHolder)*/)
			rectangleCombinations.map( elem => {
				elem.permutations.foreach { row =>
					var rectangleMask = RectangleMask(row(3), row(0), row(1), row(2))
					if(rectangleMask.validate(Rectangle.defaultPlaceHolder)) {
						resultSet += rectangleMask
					}
				}
			})
			return resultSet
		}
		def generateRectanglePermutations(list: Iterable[RectIntMask]): Iterable[Rect4IntMask] = {
			/* Buffers for left / top / right / bottom sides of rectangle */
			var buffLeft 	= new ListBuffer[RectIntMask]()
			var buffTop 	= new ListBuffer[RectIntMask]()
			var buffRight 	= new ListBuffer[RectIntMask]()
			var buffBottom 	= new ListBuffer[RectIntMask]()
			/* Buffer for left / top / right / bottom sides of rectangle mask */
			var resultSet 	= new ListBuffer[Rect4IntMask]()

			def clearBuffers(): Unit = {
				buffLeft.clear()
				buffTop.clear()
				buffRight.clear()
				buffBottom.clear()
			}
			def isNotEmpty(buffer: ListBuffer[RectIntMask]): Boolean = {
				return !buffer.isEmpty
			}

			list.filter(elem => !elem.hasPlaceholder(Rectangle.defaultPlaceHolder)).map( elem => {
				clearBuffers()
				list.map( elem2 => {
					if(elem.leftBorder == elem2.rightBorder && elem2.intersectLeft(elem).isEmpty) 			buffLeft += elem2
					else if(elem.rightBorder == elem2.leftBorder && elem2.intersectRight(elem).isEmpty) 	buffRight += elem2
					else if(elem.topBorder == elem2.bottomBorder && elem2.intersectTop(elem).isEmpty) 		buffTop += elem2
					else if(elem.bottomBorder == elem2.topBorder && elem2.intersectBottom(elem).isEmpty) 	buffBottom += elem2
					// if(elem.lTop == elem2.rBottom && elem2.intersect(elem).size == 1) 	buffTopLeft += elem2
					// else if(elem.rTop == elem2.lBottom && elem2.intersect(elem).size == 1) 	buffTopRight += elem2
					// else if(elem.lBottom == elem2.rTop && elem2.intersect(elem).size == 1) 	buffBottomLeft += elem2
					// else if(elem.rBottom == elem2.lTop && elem2.intersect(elem).size == 1) 	buffBottomRight += elem2
					// if(elem.leftBorder == elem2.rightBorder && elem2.intersectLeft(elem).isEmpty) buffLeft += elem2
				})
				if(isNotEmpty(buffLeft) && isNotEmpty(buffRight) && isNotEmpty(buffTop) && isNotEmpty(buffBottom)) {
					//println(buffTopLeft.length + " : " + buffTopRight.length + " : " + buffBottomLeft.length + " : " + buffBottomRight.length + " : " + buffLeft.length)
					//var buffLeftRight = new ListBuffer[Rect2IntMask]()
					// buffBottomLeft.map(elemBottomLeft => {
					//  	buffTopLeft.map(elemTopLeft => {
					//  		if(elemTopLeft.intersect(elemBottomLeft).isEmpty) {
					// 			buffLeft.map(elemLeft => {
					// 				if(elemLeft.intersectBottom(elemTopLeft).isEmpty && elemLeft.intersectTop(elemBottomLeft).isEmpty) {
					// 				 	count += 1
					// 				}
					// 			})
					// 		}
					//  		// buffBottomLeft.map(elemBottomLeft => {
					//  		// 	if(elemTopLeft.intersect(elemBottomLeft).isEmpty && elemLeft.intersectBottom(elemTopLeft).isEmpty && elemLeft.intersectTop(elemBottomLeft).isEmpty) {
					// 			// 	println("ok")
					// 			// }
					//  		// })
					//  	})
					// })

					//println(count)
					// var buffLeftRight = new ListBuffer[Rect2IntMask]()
					// buffLeft.map(elemLeft => {
					//  	buffRight.map(elemRight => {
					// 		if(elemLeft.intersect(elemRight).isEmpty) {
					// 			buffLeftRight += Rectangle2Mask(elemLeft, elemRight)
					// 		}
					//  	})
					// })
					// var buffTopBottom = new ListBuffer[Rect2IntMask]()
					// buffTop.map(elemTop => {
					//  	buffBottom.map(elemBottom => {
					// 		if(elemTop.intersect(elemBottom).isEmpty) {
					// 			buffTopBottom += Rectangle2Mask(elemTop, elemBottom)
					// 		}
					//  	})
					// })
					// buffLeftRight.map(elemLeftRight => {
					// 	buffTopBottom.map(elemTopBottom => {
					// 		if(elemLeftRight.intersectCenter(elemTopBottom).size == 4) {
					// 			//println(elemLeftRight + " : " + elemTopBottom)
					// 			count += 1
					// 		}
					// 	})
					// })
					buffLeft.map(elemLeft => {
						buffRight.map(elemRight => {
							if(elemLeft.intersect(elemRight, (str: String) => str != Rectangle.defaultPlaceHolder.ID).isEmpty) {
								buffTop.map(elemTop => {
									buffBottom.map(elemBottom => {
										if(elemTop.intersect(elemBottom, (str: String) => str != Rectangle.defaultPlaceHolder.ID).isEmpty &&
											elemLeft.intersectLeft(elemTop).isEmpty &&
											elemTop.intersectTop(elemRight).isEmpty &&
											elemRight.intersectRight(elemBottom).isEmpty &&
											elemBottom.intersectBottom(elemLeft).isEmpty) {

											// val matrixDimension = 4//getMaxPowerOf2(rectangleList.length)
											// var matrix = CMatrix[RectInt, Int](matrixDimension, matrixDimension)
											// matrix.fill(rectangleList, Rectangle.defaultPlaceHolder)
											// if(matrix.validate(Rectangle.defaultPlaceHolder)) {
											// 	resultSet += matrix
											// }
											var rectangle4Mask = Rectangle4Mask(elemLeft, elemTop, elemRight, elemBottom)
											if(rectangle4Mask.validate(Rectangle.defaultPlaceHolder)) {
												resultSet += rectangle4Mask
											}
										}
									})
								})
							}
						})
					})
				}
			})
			return resultSet
		}
		def showRectangleResultSet(list: Iterable[Rect4IntMask]): Unit = {
			var list = List(CMatrix[RectInt, Int](3, 3))
			if(null == list || list.isEmpty) {
				println("Cannot find possible rectangle combinations, please check possible matrix options / input data set")
				return
			}
			list.map(elem => {
				println(elem)
			})
		}

		var rectangleMatrix = createRectangleMatrix()
		var rectCombiList = generateRectangleCombinations(rectangleMatrix)
		var rectPermList = generateRectanglePermutations(rectCombiList)
		showRectangleResultSet(rectPermList)
	// var bufferTop = new ListBuffer[Rect2IntMask]()
	// var bufferBottom = new ListBuffer[Rect2IntMask]()
	// 	buffer.map(elem => {
	// 		buffer.map(elem2 => {
	// 			if(elem.topBorder == elem2.bottomBorder && elem2.intersectTop(elem).isEmpty) 			bufferTop += elem2
	// 			else if(elem.bottomBorder == elem2.topBorder && elem2.intersectBottom(elem).isEmpty) 	bufferBottom += elem2
	// 		})
	// 	})
	// 	println(bufferTop.length + " : " + bufferBottom.length)
		//println(rectangleMaskSet.length)
    }

    def nextOption(map: OptionMap, list: List[String]): OptionMap = {
      	def isSwitch(s: String) = (s(0) == '-')
	    list match {
	       	case Nil => map
	       	case "--file" :: value :: tail => nextOption(map ++ Map('file -> value.toString), tail)
	       	case "--pattern" :: value :: tail => nextOption(map ++ Map('pattern -> value.toString), tail)
	       	case "--verbose" :: tail => nextOption(map ++ Map('verbose -> true), tail)
	       	case string :: opt2 :: tail if isSwitch(opt2) => nextOption(map ++ Map('infile -> string), list.tail)
	       	case string :: Nil =>  nextOption(map ++ Map('infile -> string), list.tail)
	       	case option :: tail => println("Unknown option = " + option); return Map()
	    }
	}

	def getFileContent(is: java.io.InputStream): BufferedSource = {
    	return Source.fromInputStream(is)(decoder)
    }

	def getFileContent(file: java.io.File): List[String] = {
		if (!file.exists() || !file.isFile()) {
			return Nil
		}
    	return Source.fromFile(file)(decoder).getLines.to[List].filter(_.trim != "")
    }

	def loadDataFromFile[A] (fileName: String, predicat: (String) => Boolean, process: (String) => A): List[A] = {
		var result = new ListBuffer[A]()
		var file = new java.io.File(fileName)
		if(!file.exists() && !file.isFile()) {
			return Nil
		}
		for (line <- Source.fromFile(file)(decoder).getLines) {
			if (predicat(line)) {
				result += process(line)
		    }
		}
		return result.to[List]
	}

	def toInt(s: String): Option[Int] = {
  		try {
	    	Some(s.toInt)
  		} catch {
    		case ex: Exception => None
  		}
	}

	def duplicates[A] (list: Iterable[A]): Iterable[A] = {
    	return list.groupBy(identity).collect { case (x, List(_,_,_*)) => x }
    }

    def getMaxPowerOf2(n: Int): Int = {
    	var p: Int = 0;
    	var m = n
    	while(m > 0) {
    		p += 1
    		m >>= 1
    	}
    	return p
    }
}

class CMatrix[T >: Null <: Shape[T, S], S <: Any](numRows: Int = 1, numCols: Int = 1) {
	import utils.ConditionCheck._
	import scala.collection.mutable.ListBuffer
	/* Matrix type aliases */
	type Row = List[T]
	type Matrix = List[Row]

	private var matrix: Matrix = init(numRows, numCols) { (i: Int, j: Int) => { null } }

	/* public methods */
	def apply(i: Int, j: Int) = this.matrix(i)(j)
	def this() = this(0, 0)
	def T: Unit = transpose(this.matrix)
	def rowCount: Int = this.matrix.length
 	def colCount: Int = this.matrix.head.length
 	def size: Int = rowCount * colCount
 	def isSquare: Boolean = (rowCount == colCount)
 	def minor(i: Int, j: Int): Unit = minorMatrix(matrix, i, j)
 	def beside(matrix2: CMatrix[T, S]): Unit = this.matrix.zip( matrix2.matrix ).map{ t:(Row, Row) => t._1 ::: t._2 }
  	def above(matrix2: CMatrix[T, S]): Unit = this.matrix ::: matrix2.matrix
  	
  	def filterRows(filter: (Row) => Boolean): Matrix = filterBy[Row](this.matrix, filter)
  	def sortRows(sort: (Row, Row) => Boolean): Matrix = sortBy[Row](this.matrix, sort)
  	
  	def permutate(rowFilter: (Row) => Boolean = (Row) => true, elemPreFilter: (T) => Boolean = (T) => true, elemPostFilter: (Row) => Boolean = (Row) => true): Matrix = permutateMatrix(this.matrix, rowFilter, elemPreFilter, elemPostFilter)
  	def combinate(size: Int, rowFilter: (Row) => Boolean = (Row) => true, elemPreFilter: (T) => Boolean = (T) => true, elemPostFilter: (Row) => Boolean = (Row) => true): Matrix = combinateMatrix(this.matrix, size, rowFilter, elemPreFilter, elemPostFilter)
 	
 	def *(matrix2: CMatrix[T, S]): Unit 	= multiplyMatrices(this.matrix, matrix2.matrix)
 	def *(value: S): Unit 					= multiply(this.matrix, value)
 	def +(matrix2: CMatrix[T, S]): Unit 	= add(this.matrix, matrix2.matrix)
 	def -(matrix2: CMatrix[T, S]): Unit 	= substract(this.matrix, matrix2.matrix)

 	def update(matrix2: Matrix): Unit 	= this.matrix = matrix2
 	def fill(value: T): Unit = this.foreach( (t: T) => { value } )

	def fill(list: List[T], defaultValue: T = null): Unit = {
		def insertPlaceHolder(buffer: ListBuffer[T], delta: Int, value: T): Unit = {
			var c = 0
			while(c < delta) {
				buffer += value;
				c += 1
			}
		}
		var result = new ListBuffer[T]()
		if (this.size > list.length) {
			var delta = (this.size - list.length) / 2
			insertPlaceHolder(result, delta / 2, defaultValue)
			var (left, right) = list.splitAt(colCount - delta)
			left.map(result += _)
			insertPlaceHolder(result, (delta - delta / 2), defaultValue)
			var last = (right.length % colCount)
			var (left_, right_) = right.splitAt(right.length - last)
			left_.map(result += _)
			insertPlaceHolder(result, (this.size - list.length - delta) / 2, defaultValue)
			right_.map(result += _)
			insertPlaceHolder(result, (this.size - list.length - delta) / 2, defaultValue)
		} else {
			list.take(this.size).map(result += _)
		}
		result.to[List].zipWithIndex.foreach {
			case (elem, i) => this.insert(i, elem)
		}
	}
  	def foreach(f: (T) => T): Unit = {
  		this.matrix = (for(row <- this.matrix) yield
 			(for(elem <- row) yield f(elem)).to[List]
 		).to[List]
  	}
	def update(i: Int, j: Int, value: T): Unit = {
		ensure((m: Unit) => rowCount(matrix) >= i && colCount(matrix) >= j, {
			var row = this.matrix(i).updated(j, value)
			this.matrix = this.matrix.updated(i, row)
		})
	}
	def insert(i: Int, value: T): Unit = {
		this.update(i / rowCount, i % colCount, value)
	}
	def runTask(predicat: (T, T) => Boolean): Unit = {
		def isValid(i1: Int, j1: Int, i2: Int, j2: Int): Boolean = {
			if (rowCount > i1 && colCount > j1 && rowCount > i2 && colCount > j2) {
				var r1 = this.matrix(i1)(j1)
				var r2 = this.matrix(i2)(j2)
				return predicat(r1, r2)
			}
			return false
		}
		this.matrix.zipWithIndex.foreach {
			case(elem, i) => {
				elem.zipWithIndex.foreach {
					case(elem2, j) => {
						if(isValid(i, j, i+1, j+1)) {
							println(elem2)
						}
					}
				}
			}
		}
	}

	// private methods
	private def permutate[A] (list: List[A]): List[List[A]] = {
	    list match {
		   case List(elem) => List(List(elem))
		   case l => {
			    for {
			       i <- List.range(0, l.length)
			       p <- permutate(l.slice(0, i) ++ l.slice(i + 1, l.length))
			    } yield l(i) :: p
			}
		}
	}

	private def filterBy[A] (list: List[A], f: (A) => Boolean): List[A] = {
		return list.filter(f(_))
	}

	private def sortBy[A] (list: List[A], s: (A, A) => Boolean): List[A] = {
		return list.sortWith(s(_, _))
	}

	private def init(numRows: Int, numCols: Int) (f: (Int, Int) => T): Matrix = {
		(for(i <- 1 to numRows) yield
 			(for(j <- 1 to numCols) yield f(i, j)).to[List]).to[List]
	}

	private def multiplyRows(row1: Row, row2: Row): T = {
		requireEquals(row1.length, row2.length)
		row1.zip(row2).map { t: (T, T) => t._1 * t._2 }.reduceLeft(_ + _)
	}

	private def transpose(matrix: Matrix): Matrix = {
	 	if( matrix.head.isEmpty ) {
	 		return null
	 	}
	 	return matrix.map(_.head) :: transpose(matrix.map(_.tail))
	}

	private def multiply(matrix: Matrix, value: S): Unit = {
		matrix.map( _.map( _ * value ) )
	}

	private def multiply(matrix: Matrix, row: Row): Unit = {
	    requireEquals( rowCount(matrix), row.length )
	    matrix.map { multiplyRows(_, row) } reduceLeft ( _ + _ )
	}

 	private def multiplyMatrices(matrix1: Matrix, matrix2: Matrix): Unit = {
 		requireEquals( colCount(matrix1), rowCount(matrix2) )
    	ensure((m: Matrix) => rowCount(m) == rowCount(matrix1) && colCount(m) == colCount(matrix2),
		 	for( m1row <- matrix1 ) yield
		 	for( m2col <- transpose(matrix2) ) yield
		 		multiplyRows( m1row, m2col )
		)
	}

	private def addRows(row1: Row, row2: Row): Unit = {
		requireEquals(row1.length, row2.length)
    	row1.zip(row2).map { items: (T, T) =>
    		items._1 + items._2
    	}
	}

	private def add(matrix1: Matrix, matrix2: Matrix): Unit = {
		matrix1.zip(matrix2).map{ rows: (Row, Row) =>
			this.addRows(rows._1, rows._2)
	  	}
	}

	private def substractRows(row1: Row, row2: Row): Unit = {
		requireEquals(row1.length, row2.length)
    	row1.zip(row2).map { items: (T, T) =>
    		items._1 - items._2
    	}
	}

	private def substract(matrix1: Matrix, matrix2: Matrix): Unit = {
		matrix1.zip(matrix2).map { rows: (Row, Row) =>
			this.substractRows(rows._1, rows._2)
	  	}
	}

	private def rowCount(matrix: Matrix): Int = {
		return matrix.length
	}

 	private def colCount(matrix: Matrix): Int = {
 		return matrix.head.length
 	}

 	private def minorMatrix(matrix: Matrix, i: Int, j: Int): Unit = {
    	reduce(matrix, i).map{ reduce(_, j) }
  	}

  	private def reduce[A] (list: List[A], i: Int): List[A] = {
	    if ( i == 0 ) {
	      	list.tail
	    } else {
	    	val n = list.length
	      	assert(n >= 2)
	      	assert(0 <= i && i < n)
	      	if( i == n-1 ) {
				list.dropRight(1)
			} else {
				list.dropRight(n - i) ::: list.drop(i + 1)
			}
		}
  	}

  	private def permutateMatrix(matrix: Matrix, f1: (Row) => Boolean, f2: (T) => Boolean, f3: (Row) => Boolean): Matrix = {
  		return matrix.filter(f1).flatten.filter(f2).permutations.filter(f3).to[List]
  	}

  	private def combinateMatrix(matrix: Matrix, size: Int, f1: (Row) => Boolean, f2: (T) => Boolean, f3: (Row) => Boolean): Matrix = {
  		return matrix.filter(f1).flatten.filter(f2).combinations(size).filter(f3).to[List]
  	}

  	private def getDuplicates[A] (list: List[A], f1: (A) => Boolean): Iterable[A] = {
  		return list.filter(f1).groupBy(identity(_)).collect { case (x, List(_,_,_*)) => x }
  	}

  	private def flatten(list: List[_]): List[_] = {
  		list flatMap {
          	case list1: List[_] => flatten(list1)
          	case otherwise => List(otherwise)
    	}
    }

    private def swapByIndex(i1: Int, j1: Int, i2: Int, j2: Int): Unit = {
    	var (temp1, temp2) = (this.matrix(i1)(j1), this.matrix(i2)(j2))
    	this.update(i1, j1, temp2)
    	this.update(i2, j2, temp1)
	}

    private def swapByValue[T] (elem1: T, elem2: T, list: List[T]): List[T] = {
    	list map {
			case item if item == elem1 => elem2
    		case item if item == elem2 => elem1
    		//case item: List[T] => swap(elem1, elem2, item)
    		case item => item
		}
	}

	override def toString = "\n" + this.matrix.map {_.map{_ + " "}.reduceLeft(_ + _) + "\n"}.reduceLeft(_ + _)
}

object CMatrix {
	implicit def apply[T >: Null <: Shape[T, S], S <: Any](numRows: Int, numCols: Int): CMatrix[T, S] = new CMatrix[T, S](numRows, numCols)
}

abstract class Shape[T <: Shape[T, S], S] {
	def sum: S
	def *(value: S): T
	def *(shape2: T): T
	def +(shape2: T): T
 	def -(shape2: T): T
 	def filter(shapes: List[T]) (f: (T) => Boolean): List[T]
}

class Rectangle[T >: Int <: Int] (
	private var leftBottom: T,
	private var leftTop: T,
	private var rightTop: T,
	private var rightBottom: T) extends Shape[Rectangle[T], Int] { //extends Comparable[Rectangle];

	private val uuid: String = java.util.UUID.randomUUID.toString

	def this() 		= this(0, 0, 0, 0)
	def lBottom: T 	= leftBottom
	def lTop: T 	= leftTop
	def rTop: T 	= rightTop
	def rBottom: T 	= rightBottom
	def ID: String 	= uuid
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

	// override def compareTo(rectangle: Rectangle[T]) = {
	//     val lBottom = this.leftBottom compareTo rectangle.leftBottom
	//     if (lBottom != 0) lBottom
	//     else this.leftTop compareTo rectangle.leftTop
 	// }
	override def toString = s"\n$lTop $rTop $lBottom $rBottom"
}

object Rectangle extends AppType {
	private val placeHolder = new Rectangle[Int](0, 0, 0, 0)

	implicit def apply(leftBottom: Int, leftTop: Int, rightTop: Int, rightBottom: Int) = init(leftBottom, leftTop, rightTop, rightBottom)
	def init(leftBottom: Int, leftTop: Int, rightTop: Int, rightBottom: Int): RectInt = new Rectangle[Int](leftBottom, leftTop, rightTop, rightBottom)
	def defaultPlaceHolder: RectInt = placeHolder
}

abstract class ShapeMask[T <: ShapeMask[T, S], S <: Any] {
 	def diff(shapeMask2: T, f: (S) => Boolean = (S) => true): Set[S]
 	def intersect(shapeMask2: T, f: (S) => Boolean = (S) => true): Set[S]

 	protected def tuple2ToSet[A] (t: (A, A)): Set[A] 		= Set(t._1, t._2)
 	protected def tuple3ToSet[A] (t: (A, A, A)): Set[A] 	= Set(t._1, t._2, t._3)
	protected def tuple4ToSet[A] (t: (A, A, A, A)): Set[A] 	= Set(t._1, t._2, t._3, t._4)
}

class RectangleMask[T >: Null <: Rectangle[Int]] (
	private var leftBottom: T,
	private var leftTop: T,
	private var rightTop: T,
	private var rightBottom: T) extends ShapeMask[RectangleMask[T], String] {

	def this() = this(null, null, null, null)
	def lBottom: T 	= leftBottom
	def lTop: T 	= leftTop
	def rTop: T 	= rightTop
	def rBottom: T 	= rightBottom
	def hasPlaceholder(rectShape: T = null): Boolean = this.borderSet.contains(getShapeID(rectShape))

	def cValue: Int = (leftTop.rBottom + rightTop.lBottom + rightBottom.lTop + leftBottom.rTop)
	def lValue: Int = (leftTop.lBottom + leftBottom.lTop)
	def tValue: Int = (leftTop.rTop + rightTop.lTop)
	def rValue: Int = (rightTop.rBottom + rightBottom.rTop)
	def bValue: Int = (rightBottom.lBottom + leftBottom.rBottom)

	def intersectLeft(rectangleMask2: RectangleMask[T]): Set[String] 	= this.intersectMask[String](this.leftBorderSet, rectangleMask2.borderSet)
	def intersectRight(rectangleMask2: RectangleMask[T]): Set[String] 	= this.intersectMask[String](this.rightBorderSet, rectangleMask2.borderSet)
	def intersectTop(rectangleMask2: RectangleMask[T]): Set[String] 	= this.intersectMask[String](this.topBorderSet, rectangleMask2.borderSet)
	def intersectBottom(rectangleMask2: RectangleMask[T]): Set[String] 	= this.intersectMask[String](this.bottomBorderSet, rectangleMask2.borderSet)

	def leftBorder: Tuple2[String, String] 								= Tuple2(getShapeID(leftBottom), getShapeID(leftTop))
	def topBorder: Tuple2[String, String] 								= Tuple2(getShapeID(leftTop), getShapeID(rightTop))
	def rightBorder: Tuple2[String, String] 							= Tuple2(getShapeID(rightBottom), getShapeID(rightTop))
	def bottomBorder: Tuple2[String, String] 							= Tuple2(getShapeID(leftBottom), getShapeID(rightBottom))
	def border: Tuple4[String, String, String, String] 					= Tuple4(getShapeID(leftTop), getShapeID(rightTop), getShapeID(leftBottom), getShapeID(rightBottom))

	val leftBorderSet: Set[String] 		= tuple2ToSet[String](this.leftBorder)
	val topBorderSet: Set[String] 		= tuple2ToSet[String](this.topBorder)
	val rightBorderSet: Set[String] 	= tuple2ToSet[String](this.rightBorder)
	val bottomBorderSet: Set[String] 	= tuple2ToSet[String](this.bottomBorder)
	val borderSet: Set[String] 			= topBorderSet.union(bottomBorderSet)

	override def diff(rectangleMask2: RectangleMask[T], f: (String) => Boolean = (String) => true): Set[String] = {
		return this.borderSet.filter(f(_)).diff(rectangleMask2.borderSet)
	}
	override def intersect(rectangleMask2: RectangleMask[T], f: (String) => Boolean = (String) => true): Set[String] = {
		return this.intersectMask[String](this.borderSet.filter(f(_)), rectangleMask2.borderSet)
	}

	def validate(placeHolder: T = null): Boolean = {
		def isPerimeterValid(): Boolean = {
			return (this.lValue <= 10 && this.tValue <= 10 && this.rValue <= 10 && this.bValue <= 10)
		}
		def isFullValid(): Boolean = {
			if(this.cValue == 10 && isPerimeterValid())
				return true
			return false
		}
		def isPartialValid(): Boolean = {
			if(this.cValue <= 10 && isPerimeterValid())
				return true
			return false
		}
		if(this.hasPlaceholder(placeHolder))
			return isPartialValid()
		return isFullValid()
	}

	private def getShapeID(rectShape: T): String = {
		if (null == rectShape)
			return null
		return rectShape.ID
	}
	private def intersectMask[A] (elemBorders: Set[A], elem2Borders: Set[A]): Set[A] = elemBorders.intersect(elem2Borders)

	override def toString = s"\n{ RectangleMask => \nleftTop: ($lTop), \nrightTop: ($rTop), \nleftBottom: ($lBottom), \nrightBottom: ($rBottom) }"
}

object RectangleMask extends AppType {
	implicit def apply(leftBottom: RectInt, leftTop: RectInt, rightTop: RectInt, rightBottom: RectInt) = init(leftBottom, leftTop, rightTop, rightBottom)
	def init(leftBottom: RectInt, leftTop: RectInt, rightTop: RectInt, rightBottom: RectInt): RectangleMask[RectInt] = new RectangleMask[RectInt](leftBottom, leftTop, rightTop, rightBottom)
}

class Rectangle2Mask[T >: Null <: RectangleMask[Rectangle[Int]]] (
	private var leftBottom: T,
	private var leftTop: T,
	private var rightTop: T,
	private var rightBottom: T) extends ShapeMask[Rectangle2Mask[T], String] {

	def this() = this(null, null, null, null)
	def lBottom: T 	= leftBottom
	def lTop: T 	= leftTop
	def rTop: T 	= rightTop
	def rBottom: T 	= rightBottom

	def intersectLeft(rectangle2Mask2: Rectangle2Mask[T]): Set[String] 		= this.intersectMask[String](this.leftBorderSet, rectangle2Mask2.borderSet)
	def intersectRight(rectangle2Mask2: Rectangle2Mask[T]): Set[String] 	= this.intersectMask[String](this.rightBorderSet, rectangle2Mask2.borderSet)
	def intersectTop(rectangle2Mask2: Rectangle2Mask[T]): Set[String]	 	= this.intersectMask[String](this.topBorderSet, rectangle2Mask2.borderSet)
	def intersectBottom(rectangle2Mask2: Rectangle2Mask[T]): Set[String]	= this.intersectMask[String](this.bottomBorderSet, rectangle2Mask2.borderSet)

	def leftBorder: Tuple4[String, String, String, String] 		= Tuple4(getShapeID(leftBottom.lTop), getShapeID(leftBottom.lBottom), getShapeID(leftTop.lTop), getShapeID(leftTop.lBottom))
	def topBorder: Tuple4[String, String, String, String]		= Tuple4(getShapeID(leftTop.lTop), getShapeID(leftTop.rTop), getShapeID(rightTop.lTop), getShapeID(rightTop.rTop))
	def rightBorder: Tuple4[String, String, String, String] 	= Tuple4(getShapeID(rightBottom.rTop), getShapeID(rightBottom.rBottom), getShapeID(rightTop.rTop), getShapeID(rightTop.rBottom))
	def bottomBorder: Tuple4[String, String, String, String]	= Tuple4(getShapeID(rightBottom.rBottom), getShapeID(rightBottom.lBottom), getShapeID(leftBottom.lBottom), getShapeID(leftBottom.rBottom))
	def center: Tuple4[String, String, String, String]			= Tuple4(getShapeID(leftBottom.rTop), getShapeID(leftTop.rBottom), getShapeID(rightTop.lBottom), getShapeID(rightBottom.lTop))

	val leftBorderSet: Set[String] 			= tuple4ToSet[String](this.leftBorder)
	val topBorderSet: Set[String] 			= tuple4ToSet[String](this.topBorder)
	val rightBorderSet: Set[String] 		= tuple4ToSet[String](this.rightBorder)
	val bottomBorderSet: Set[String] 		= tuple4ToSet[String](this.bottomBorder)
	val centerSet: Set[String]				= tuple4ToSet[String](this.center)
	val borderSet: Set[String]				= topBorderSet.union(bottomBorderSet).union(leftBorderSet).union(rightBorderSet)

	override def diff(rectangle2Mask2: Rectangle2Mask[T], f: (String) => Boolean = (String) => true): Set[String] = {
		return this.borderSet.filter(f(_)).diff(rectangle2Mask2.borderSet)
	}
	override def intersect(rectangle2Mask2: Rectangle2Mask[T], f: (String) => Boolean = (String) => true): Set[String] = {
		return this.intersectMask[String](this.borderSet.filter(f(_)), rectangle2Mask2.borderSet)
	}

	private def getShapeID(shape: Rectangle[Int]): String = {
		if (null == shape)
			return null
		return shape.ID
	}
	private def intersectMask[A] (elemBorders: Set[A], elem2Borders: Set[A]): Set[A] = elemBorders.intersect(elem2Borders)

	override def toString = s"\n{ Rectangle2Mask => \n\tleftTop: ($lTop), \n\trightTop: ($rTop), \n\tleftBottom: ($lBottom), \n\trightBottom: ($rBottom) }"
}

object Rectangle2Mask extends AppType {
	implicit def apply(leftBottom: RectMaskInt, leftTop: RectMaskInt, rightTop: RectMaskInt, rightBottom: RectMaskInt) = init(leftBottom, leftTop, rightTop, rightBottom)
	def init(leftBottom: RectMaskInt, leftTop: RectMaskInt, rightTop: RectMaskInt, rightBottom: RectMaskInt): Rectangle2Mask[RectMaskInt] = new Rectangle2Mask[RectMaskInt](leftBottom, leftTop, rightTop, rightBottom)
}

class Rectangle4Mask[T >: Null <: RectangleMask[Rectangle[Int]]] (
	private var left: T,
	private var top: T,
	private var right: T,
	private var bottom: T) extends ShapeMask[Rectangle4Mask[T], String] {

	def this() = this(null, null, null, null)
	def lTop: RectangleMask[Rectangle[Int]] 	= RectangleMask(left.lTop, Rectangle.defaultPlaceHolder, top.lTop, left.rTop)
	def rTop: RectangleMask[Rectangle[Int]] 	= RectangleMask(right.lTop, top.rTop, Rectangle.defaultPlaceHolder, right.rTop)
	def rBottom: RectangleMask[Rectangle[Int]] 	= RectangleMask(bottom.rBottom, right.lBottom, right.rBottom, Rectangle.defaultPlaceHolder)
	def lBottom: RectangleMask[Rectangle[Int]] 	= RectangleMask(Rectangle.defaultPlaceHolder, left.lBottom, left.rBottom, bottom.lBottom)

	def intersectLeft(rectangle2Mask2: Rectangle4Mask[T]): Set[String] 		= this.intersectMask[String](this.leftBorderSet, rectangle2Mask2.borderSet)
	def intersectRight(rectangle2Mask2: Rectangle4Mask[T]): Set[String] 	= this.intersectMask[String](this.rightBorderSet, rectangle2Mask2.borderSet)
	def intersectTop(rectangle2Mask2: Rectangle4Mask[T]): Set[String]	 	= this.intersectMask[String](this.topBorderSet, rectangle2Mask2.borderSet)
	def intersectBottom(rectangle2Mask2: Rectangle4Mask[T]): Set[String]	= this.intersectMask[String](this.bottomBorderSet, rectangle2Mask2.borderSet)

	def leftBorder: Tuple2[String, String] 				= Tuple2(getShapeID(left.lTop), getShapeID(left.lBottom))
	def topBorder: Tuple2[String, String]				= Tuple2(getShapeID(top.lTop), getShapeID(top.rTop))
	def rightBorder: Tuple2[String, String] 			= Tuple2(getShapeID(right.rTop), getShapeID(right.rBottom))
	def bottomBorder: Tuple2[String, String]			= Tuple2(getShapeID(bottom.lBottom), getShapeID(bottom.rBottom))
	def center: Tuple4[String, String, String, String] 	= Tuple4(getShapeID(left.rTop), getShapeID(left.rBottom), getShapeID(right.lTop), getShapeID(right.lBottom))

	def leftTopBorder: Tuple3[String, String, String] 		= Tuple3(getShapeID(left.lTop), getShapeID(left.rTop), getShapeID(top.lTop))
	def rightTopBorder: Tuple3[String, String, String] 		= Tuple3(getShapeID(right.lTop), getShapeID(right.rTop), getShapeID(top.rTop))
	def leftBottomBorder: Tuple3[String, String, String] 	= Tuple3(getShapeID(left.lBottom), getShapeID(left.rBottom), getShapeID(bottom.lBottom))
	def rightBottomBorder: Tuple3[String, String, String] 	= Tuple3(getShapeID(right.lBottom), getShapeID(right.rBottom), getShapeID(bottom.rBottom))

	val leftBorderSet: Set[String] 			= tuple2ToSet[String](this.leftBorder)
	val topBorderSet: Set[String] 			= tuple2ToSet[String](this.topBorder)
	val rightBorderSet: Set[String] 		= tuple2ToSet[String](this.rightBorder)
	val bottomBorderSet: Set[String] 		= tuple2ToSet[String](this.bottomBorder)
	val centerSet: Set[String]				= tuple4ToSet[String](this.center)
	val borderSet: Set[String]				= leftBorderSet.union(topBorderSet).union(rightBorderSet).union(bottomBorderSet)

	override def diff(rectangle2Mask2: Rectangle4Mask[T], f: (String) => Boolean = (String) => true): Set[String] = {
		return this.borderSet.filter(f(_)).diff(rectangle2Mask2.borderSet)
	}
	override def intersect(rectangle2Mask2: Rectangle4Mask[T], f: (String) => Boolean = (String) => true): Set[String] = {
		return this.intersectMask[String](this.borderSet.filter(f(_)), rectangle2Mask2.borderSet)
	}

	def validate(placeHolder: Rectangle[Int] = null): Boolean = {
		if(lTop.validate(placeHolder) &&
			rTop.validate(placeHolder) &&
			rBottom.validate(placeHolder) &&
			lBottom.validate(placeHolder))
			return true
		return false
	}

	private def getShapeID(shape: Rectangle[Int]): String = {
		if (null == shape)
			return null
		return shape.ID
	}
	private def intersectMask[A] (elemBorders: Set[A], elem2Borders: Set[A]): Set[A] = elemBorders.intersect(elem2Borders)

	override def toString = s"\n{ Rectangle4Mask => \n\tleft: ($left), \n\ttop: ($top), \n\tright: ($right), \n\tbottom: ($bottom) }"
}

object Rectangle4Mask extends AppType {
	implicit def apply(left: RectMaskInt, top: RectMaskInt, right: RectMaskInt, bottom: RectMaskInt) = init(left, top, right, bottom)
	def init(left: RectMaskInt, top: RectMaskInt, right: RectMaskInt, bottom: RectMaskInt): Rectangle4Mask[RectMaskInt] = new Rectangle4Mask[RectMaskInt](left, top, right, bottom)
}