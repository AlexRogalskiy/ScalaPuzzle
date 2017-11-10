package com.wildbeeslabs

object Puzzle extends AppInitializer {
	/* Default usage mapping */
	val usage = """
		Usage: puzzle --file filename
	"""
	def main(args: Array[String]): Unit = {
    	if (args.length == 0) {
	        println(usage)
	        return//sys.exit(0)
	    }
    	val options = nextOption(Map().withDefaultValue("Not found"), args.to[List])
    	process(options)
    }
}

trait AppInitializer {
	import scala.io.Codec
	import scala.io.Source
	import scala.io.BufferedSource
	import scala.collection.mutable.ListBuffer

	implicit val decoder = Codec.UTF8.decoder
	decoder.onMalformedInput(java.nio.charset.CodingErrorAction.IGNORE)
	decoder.onUnmappableCharacter(java.nio.charset.CodingErrorAction.IGNORE)

	type OptionMap = Map[Symbol, Any]

	def process(map: OptionMap): Unit = {
	// val a = Array(2, 3, 5, 7, 11) 
		// val result = for (elem <- a if elem % 2 == 0) yield 2 * elem
		// for (elem <- result)  println(elem)
		// val positionsToRemove = for (i <- a.indices if a(i) < 0) yield i 

		// val triangle = new Array[Array[Int]](12)
		// for (i <- triangle.indices)
		// 	triangle(i) = new Array[Int](i + 1)

		var list = loadDataFromFile[List[Int]](
					map.getOrElse('file, "default.txt").toString,
					(str: String) => { str.trim.matches("\\d{1}\\s+\\d\\s+\\d\\s+\\d") },
					(str: String) => { str.split("\\s+").map((value) => toInt(value.trim).getOrElse(0)).toList }
		)
		var rectList = list.map((elem) => new Rectangle(elem(2), elem(0), elem(1), elem(3)))
		val matrixDim = getDimension(rectList.length)
		var rectMatrix = CMatrix[Rectangle](matrixDim, matrixDim)
		// var data = (
		// 	for(i <- 1 to matrixDim) yield
	 // 			(for(j <- 1 to matrixDim) yield new Rectangle(1, 2, 3, 4)).toList
	 // 	).toList
		// var data = (
		// 	for(i <- 1 to 13) yield new Rectangle(1, 2, 3, 4)
		// ).toList
		rectMatrix.fill(rectList)
		//rectMatrix.update(1, 1, null)
		// val A = CMatrix(matrixDim, matrixDim) { (i: Int, j: Int) => {
		// 		new Rectangle(1, 2, 3, 4)
	 // 			//if(i == j) 1 else 0
	 // 		}
		// }
		// println(A)
		// rectMatrix.fill(A)
		//println(data.take(1))
		println(rectMatrix)
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
    	return Source.fromFile(file)(decoder).getLines.to[List]
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

    def getDimension(n: Int): Int = {
    	var p: Int = 0;
    	var m = n
    	while(m > 0) {
    		p += 1
    		m >>= 1
    	}
    	return p
    }
}

class CMatrix[T >: Null <: Shape[T]](numRows: Int = 1, numCols: Int = 1) {
	import utils.ConditionCheck._
	import scala.collection.mutable.ListBuffer
	/* Matrix type aliases */
	type Row = List[T]
	type Matrix = List[Row]

	private var matrix: Matrix = init(numRows, numCols) { (i: Int, j: Int) => { null } }
	//private var matrix = Array.ofDim[Shape] (numRows, numCols)
	//private var matrix: Option[Array[Array[Shape]]] = None

	/* public methods */
	def apply(i: Int, j: Int) = this.matrix(i)(j)
	def this() = this(0, 0)
	def T: Unit = transpose(this.matrix)
	def rowCount: Int = this.matrix.length
 	def colCount: Int = this.matrix.head.length
 	def size: Int = rowCount * colCount
 	def isSquare: Boolean = (rowCount == colCount)
 	def minor(i: Int, j: Int): Unit = minorMatrix(matrix, i, j)
 	def beside(matrix2: CMatrix[T]): Unit = this.matrix.zip( matrix2.matrix ).map{ t:(Row, Row) => t._1 ::: t._2 }
  	def above(matrix2: CMatrix[T]): Unit = this.matrix ::: matrix2.matrix
 	def *(matrix2: CMatrix[T]): Unit = multiplyMatrices(this.matrix, matrix2.matrix)
 	def *(value: Int): Unit = multiply(this.matrix, value)
 	def +(matrix2: CMatrix[T]): Unit = add(this.matrix, matrix2.matrix)
 	def -(matrix2: CMatrix[T]): Unit = substract(this.matrix, matrix2.matrix)
 	def update(matrix2: Matrix): Unit = this.matrix = matrix2
 	def fill(value: T): Unit = matrix match {
		case matrix: Matrix => {
			this.foreach((t: T) => { value })
		}
  	}
	def fill(list: List[T]): Unit = matrix match {
		case matrix: Matrix => {
			var result = new ListBuffer[T]()
			if(this.size > list.length) {
				var delta = (this.size - list.length) / 2
				var c = 0
				while(c < delta) {
					result += null; c += 1
				}
				list.map(result += _)
				while(c <= this.size - list.length - delta) {
					result += null; c += 1
				}
			} else {
				list.take(this.size).map(result += _)
			}
			result.to[List].zipWithIndex.foreach {
				case (elem, i) => this.insert(i, elem)
			}
		}
  	}
  	def foreach(f: (T) => T): Unit = {
  		this.matrix = (for(row <- this.matrix) yield
 			(for(elem <- row) yield f(elem)).to[List]
 		).to[List]
  	}
	def sum(): Int = matrix match {
		case matrix: Matrix =>
			var res: Int = 0
		    for(row <- matrix)
		    	for(elem <- row)
		    		res += elem.sum
		    return res
	}
	def update(i: Int, j: Int, value: T): Unit = {
		ensure((m: Unit) => rowCount(matrix) >= i && colCount(matrix) >= j, {
				var row = this.matrix(i).updated(j, value)
				this.matrix = this.matrix.updated(i, row)
			}
		)
	}
	def insert(i: Int, value: T): Unit = {
		this.update(i / rowCount, i % colCount, value)
	}

	// private methods
	private def init(numRows: Int, numCols: Int) (f: (Int, Int) => T): Matrix = (
 		for(i <- 1 to numRows) yield
 			(for(j <- 1 to numCols) yield f(i, j)).to[List]
 	).to[List]

	private def multiplyRows(row1: Row, row2: Row): T = {
		requireEquals(row1.length, row2.length)
		row1.zip(row2).map{ t: (T, T) => t._1 * t._2 }.reduceLeft(_ + _)
	}

	private def transpose(matrix: Matrix): Matrix = {
	 	if( matrix.head.isEmpty ) null
	 	else matrix.map(_.head) :: transpose(matrix.map(_.tail))
	}

	private def multiply(matrix: Matrix, value: Int): Unit = {
		matrix.map( _.map( _ * value ) )
	}

	private def multiply(matrix: Matrix, row: Row): Unit = {
	    requireEquals( rowCount(matrix), row.length )
	    matrix.map{ multiplyRows(_, row) } reduceLeft ( _ + _ )
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
    	row1.zip(row2).map{ t: (T, T) =>
    		t._1 + t._2
    	}
	}

	private def add(matrix1: Matrix, matrix2: Matrix): Unit = {
		matrix1.zip(matrix2).map{ rows: (Row, Row) =>
		    rows._1.zip( rows._2 ).map{ items: (T, T) =>
		      	items._1 + items._2
		    }
	  	}
	}

	private def substractRows(row1: Row, row2: Row): Unit = {
		requireEquals(row1.length, row2.length)
    	row1.zip(row2).map{ t: (T, T) =>
    		t._1 - t._2
    	}
	}

	private def substract(matrix1: Matrix, matrix2: Matrix): Unit = {
		matrix1.zip(matrix2).map{ rows: (Row, Row) =>
		    rows._1.zip( rows._2 ).map{ items: (T, T) =>
		      	items._1 - items._2
		    }
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

  	private def flatten(list: List[_]): List[_] =
  		list flatMap {
          	case list1: List[_] => flatten(list1)
          	case otherwise => List(otherwise)
    }

    private def swap[T] (elem1: T, elem2: T, list: List[T]): List[T] =
    	list map {
			case item if item == elem1 => elem2
    		case item if item == elem2 => elem1
    		//case item: List[T] => swap(elem1, elem2, item)
    		case item => item
	}

  	// override methods
	override def toString = "\n" + this.matrix.map {_.map{"\t" + _}.reduceLeft(_ + _) + "\n"}.reduceLeft(_ + _)
}

object CMatrix {
	implicit def apply[T >: Null <: Shape[T]](numRows: Int, numCols: Int): CMatrix[T] = new CMatrix[T](numRows, numCols)
}

abstract class Shape[T <: Shape[T]] {
	def sum(): Int
	def *(shape2: T): T
	def *(value: Int): T
	def +(shape2: T): T
 	def -(shape2: T): T
 	def filter(shapes: Vector[T]) (f: (T) => Boolean): Vector[T]
}

class Rectangle (
	private var lBottom: Int,
	private var lTop: Int,
	private var rTop: Int,
	private var rBottom: Int) extends Shape[Rectangle] { //extends Comparable[Rectangle];

	def this() = this(0, 0, 0, 0)
 	override def *(rectangle2: Rectangle) = multiply(this, rectangle2)
 	override def *(value: Int) = multiply(this, value)
 	override def +(rectangle2: Rectangle) = add(this, rectangle2)
 	override def -(rectangle2: Rectangle) = substract(this, rectangle2)
	override def sum: Int = {
		return (this.lBottom + this.lTop + this.rTop + this.rBottom)
	}
	override def filter(rectangles: Vector[Rectangle]) (f: (Rectangle) => Boolean): Vector[Rectangle] = (
		for (rectangle <- rectangles; if f(rectangle))
			yield rectangle
	).to[Vector]

	private def multiply(rectangle2: Rectangle): Unit = {
		this.lBottom *= rectangle2.lBottom
		this.lTop *= rectangle2.lTop
		this.rTop *= rectangle2.rTop
		this.rBottom *= rectangle2.rBottom
	}

	private def multiply(value: Int): Unit = {
		this.lBottom *= value
		this.lTop *= value
		this.rTop *= value
		this.rBottom *= value
	}

	private def add(rectangle2: Rectangle): Unit = {
		this.lBottom += rectangle2.lBottom
		this.lTop += rectangle2.lTop
		this.rTop += rectangle2.rTop
		this.rBottom += rectangle2.rBottom
	}

 	private def substract(rectangle2: Rectangle): Unit = {
		this.lBottom -= rectangle2.lBottom
		this.lTop -= rectangle2.lTop
		this.rTop -= rectangle2.rTop
		this.rBottom -= rectangle2.rBottom
 	}

	private def multiply(rectangle1: Rectangle, rectangle2: Rectangle): Rectangle = {
		new Rectangle(rectangle1.lBottom * rectangle2.lBottom,
					  rectangle1.lTop * rectangle2.lTop,
					  rectangle1.rTop * rectangle2.rTop,
					  rectangle1.rBottom * rectangle2.rBottom
					 )
	}

	private def multiply(rectangle1: Rectangle, value: Int): Rectangle = {
		new Rectangle(rectangle1.lBottom * value,
					  rectangle1.lTop * value,
					  rectangle1.rTop * value,
					  rectangle1.rBottom * value
					 )
	}

	private def add(rectangle1: Rectangle, rectangle2: Rectangle): Rectangle = {
		new Rectangle(rectangle1.lBottom + rectangle2.lBottom,
					  rectangle1.lTop + rectangle2.lTop,
					  rectangle1.rTop + rectangle2.rTop,
					  rectangle1.rBottom + rectangle2.rBottom
					 )
	}

 	private def substract(rectangle1: Rectangle, rectangle2: Rectangle): Rectangle = {
		new Rectangle(rectangle1.lBottom - rectangle2.lBottom,
					  rectangle1.lTop - rectangle2.lTop,
					  rectangle1.rTop - rectangle2.rTop,
					  rectangle1.rBottom - rectangle2.rBottom
					 )
 	}
	// override def compareTo(rectangle: Rectangle) = {
	//     val lBottom = this.leftBottom compareTo rectangle.leftBottom
	//     if (lBottom != 0) lBottom
	//     else this.leftTop compareTo rectangle.leftTop
 	// }
	override def toString = s"{leftBottom: ($lBottom), leftTop: ($lTop), rightTop: ($rTop), rightBottom: ($rBottom)}"
}

object Rectangle {
	def apply(leftBottom: Int, leftTop: Int, rightTop: Int, rightBottom: Int) = init(leftBottom, leftTop, rightTop, rightBottom)

	def init(leftBottom: Int, leftTop: Int, rightTop: Int, rightBottom: Int) = new Rectangle(leftBottom, leftTop, rightTop, rightBottom)
}