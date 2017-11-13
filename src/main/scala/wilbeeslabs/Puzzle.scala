package com.wildbeeslabs

object Puzzle extends AppInitializer {
	/* Default usage mapping */
	val usage =
	"""
	>>> Program Information <<<

		Usage: puzzle --file filename

	>>> Copyright (C)<<<
	"""
	def main(args: Array[String]): Unit = {
		//println(List(new Rectangle(1, 2, 3, 4), new Rectangle(1, 2, 3, 4), new Rectangle(1, 2, 3, 4)).filter(_ != null).permutations.toList)
		//println(permutate[Rectangle](List(new Rectangle(1, 2, 3, 4), new Rectangle(1, 2, 3, 4), new Rectangle(1, 2, 3, 4))))

		//var lol = List(List(1), List(2), 3);
		//val lol = List(List(new Rectangle(1, 2, 3, 4), new Rectangle(1, 2, 3, 4)), List(new Rectangle(1, 2, 3, 4), new Rectangle(1, 2, 3, 4)))
		//println(lol.flatten.filter(null != _).permutations.toList)
		//println(lol.flatten.filter(null != _).combinations(2).toList)
		
		//var ls = List(1, 2, 3, 4, 5)
		//var l = ls.combinations(4).map((elem) => println(elem.permutations.length))
		//println(l.length)

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
		var rectList = list.map(elem => new Rectangle[Int](elem(2), elem(0), elem(1), elem(3)))
		val matrixDim = getMaxPowerOf2(rectList.length)
		var rectMatrix = CMatrix[Rectangle[Int]](matrixDim, matrixDim)
		rectMatrix.fill(rectList)
		// var data = (
		// 	for(i <- 1 to matrixDim) yield
	 // 			(for(j <- 1 to matrixDim) yield new Rectangle(1, 2, 3, 4)).toList
	 // 	).toList
		// var data = (
		// 	for(i <- 1 to 13) yield new Rectangle(1, 2, 3, 4)
		// ).toList

		// rectMatrix.runTask((r1, r2) => { 
		// 	(r1.rTop + r2.lTop <= 10 && r1.rBottom + r2.lBottom <= 10)
		// })

		//var cc = rectMatrix.permutate((r: List[Rectangle]) => true, (r: Rectangle) => null != r)
		//println(cc.length)

		var cc = rectMatrix.combinate(4, (r: List[Rectangle[Int]]) => true, (r: Rectangle[Int]) => (null != r), (r: List[Rectangle[Int]]) => { 
			(r(0).rBottom + r(1).lBottom + r(2).rTop + r(3).lTop) == 10 &&
			(r(0).lBottom + r(2).lTop) <= 10 &&
			(r(0).rTop + r(1).lTop) <= 10 &&
			(r(1).rBottom + r(3).rTop) <= 10 &&
			(r(2).rBottom + r(3).lBottom) <= 10
		})
		//println(cc.mkString("\n"))
		//println(cc.length)
		var cc2 = rectMatrix.combinate(4, (r: List[Rectangle[Int]]) => true, (r: Rectangle[Int]) => (null != r), (r: List[Rectangle[Int]]) => { 
			(r(0).rBottom + r(1).lBottom + r(2).lTop + r(3).rTop) == 10 &&
			(r(0).lBottom + r(3).lTop) <= 10 &&
			(r(0).rTop + r(1).lTop) <= 10 &&
			(r(1).rBottom + r(2).rTop) <= 10 &&
			(r(3).rBottom + r(2).lBottom) <= 10
		})
		//println(cc2.length)

		var cc3 = rectMatrix.combinate(4, (r: List[Rectangle[Int]]) => true, (r: Rectangle[Int]) => (null != r), (r: List[Rectangle[Int]]) => { 
			(r(3).rBottom + r(2).lBottom + r(1).lTop + r(0).rTop) == 10 &&
			(r(3).lBottom + r(0).lTop) <= 10 &&
			(r(0).rBottom + r(1).lBottom) <= 10 &&
			(r(1).rTop + r(2).rBottom) <= 10 &&
			(r(2).lTop + r(3).rTop) <= 10
		})
		//println(cc3.length)

		import scala.collection.mutable.ListBuffer
		var result = new ListBuffer[List[Rectangle[Int]]]()
		var cc4 = rectMatrix.combinate(4, (r: List[Rectangle[Int]]) => true, (r: Rectangle[Int]) => (null != r))
		cc4.map(elem => {
			elem.permutations.foreach {
				case(r) => {
					if((r(0).rBottom + r(1).lBottom + r(2).rTop + r(3).lTop) == 10 &&
					(r(0).lBottom + r(2).lTop) <= 10 &&
					(r(0).rTop + r(1).lTop) <= 10 &&
					(r(1).rBottom + r(3).rTop) <= 10 &&
					(r(2).rBottom + r(3).lBottom) <= 10) {
						result += r
					}
				}
			}
		})
		//var dups = result.toList.groupBy(identity).collect { case (x, List(_,_,_*)) => x }
		//println(result)
		//println(dups.toList.length)
		var rectMask = result.map(elem => {
			new RectangleMask(elem(0), elem(1), elem(2), elem(3))
		})
		println(rectMask)
		// import java.io._
		// var file = "result1.txt"
		// var writer = new BufferedWriter(new FileWriter(file))
		// result1.map(_.toString).foreach(writer.write)
		// writer.close()

		// file = "result.txt"
		// writer = new BufferedWriter(new FileWriter(file))
		// result.map(_.toString).foreach(writer.write)
		// writer.close()

		// file = "dups.txt"
		// writer = new BufferedWriter(new FileWriter(file))
		// dups.map(_.toString).foreach(writer.write)
		// writer.close()

		//var list_ = List(1, 2, 3, 4)
		//var dups = list_.groupBy(identity).collect { case (x, List(_,_,_*)) => x }
		//println(list_.permutations.toList)

		//rectMatrix.update(1, 1, null)
		// val A = CMatrix(matrixDim, matrixDim) { (i: Int, j: Int) => {
		// 		new Rectangle(1, 2, 3, 4)
	 // 			//if(i == j) 1 else 0
	 // 		}
		// }
		// println(A)
		// rectMatrix.fill(A)
		//println(data.take(1))
		//println(rectMatrix)
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
  	
  	def filterRows(filter: (Row) => Boolean): Matrix = filterBy[Row](this.matrix, filter)
  	def sortRows(sort: (Row, Row) => Boolean): Matrix = sortBy[Row](this.matrix, sort)
  	
  	def permutate(rowFilter: (Row) => Boolean = (Row) => true, elemPreFilter: (T) => Boolean = (T) => true, elemPostFilter: (Row) => Boolean = (Row) => true): Matrix = permutateMatrix(this.matrix, rowFilter, elemPreFilter, elemPostFilter)
  	def combinate(size: Int, rowFilter: (Row) => Boolean = (Row) => true, elemPreFilter: (T) => Boolean = (T) => true, elemPostFilter: (Row) => Boolean = (Row) => true): Matrix = combinateMatrix(this.matrix, size, rowFilter, elemPreFilter, elemPostFilter)
 	
 	def *(matrix2: CMatrix[T]): Unit = multiplyMatrices(this.matrix, matrix2.matrix)
 	def *(value: Int): Unit = multiply(this.matrix, value)
 	def +(matrix2: CMatrix[T]): Unit = add(this.matrix, matrix2.matrix)
 	def -(matrix2: CMatrix[T]): Unit = substract(this.matrix, matrix2.matrix)
 	def update(matrix2: Matrix): Unit = this.matrix = matrix2
 	def fill(value: T): Unit = {
		this.foreach((t: T) => { value })
  	}
	def fill(list: List[T]): Unit = {
		def padList(buffer: ListBuffer[T], delta: Int, value: T): Unit = {
			var c = 0
			while(c < delta) {
				buffer += value;
				c += 1
			}
		}
		var result = new ListBuffer[T]()
		if (this.size > list.length) {
			var delta = (this.size - list.length) / 2
			padList(result, delta / 2, null)
			var (left, right) = list.splitAt(colCount - delta)
			left.map(result += _)
			padList(result, (delta - delta / 2), null)
			var last = (right.length % colCount)
			var (left_, right_) = right.splitAt(right.length - last)
			left_.map(result += _)
			padList(result, (this.size - list.length - delta) / 2, null)
			right_.map(result += _)
			padList(result, (this.size - list.length - delta) / 2, null)
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
	def sum(): Int = {
		var res = 0
		for(row <- matrix)
		   	for(elem <- row)
		   		if(null != elem) {
		   			res += elem.sum
		   		}
		return res
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
	def runTask(f: (T, T) => Boolean): Int = {
		def isValid(i1: Int, j1: Int, i2: Int, j2: Int): Boolean = {
			if (rowCount > i1 && colCount > j1 && rowCount > i2 && colCount > j2) {
			//ensure((m: Unit) => rowCount(matrix) >= i1 && colCount(matrix) >= j1 && rowCount(matrix) >= i2 && colCount(matrix) >= j2, {
				var r1 = this.matrix(i1)(j1)
				var r2 = this.matrix(i2)(j2)
				if(null != r1 && null != r2) {
					return f(r1, r2)
				}
			//})
			}
			return false
		}
		this.matrix.zipWithIndex.foreach {
			case(elem, i) => {
				var count = 0
				elem.zipWithIndex.foreach {
					case(elem2, j) => {
						//if (null != elem2) {
							if(isValid(i, j, i+1, j+1)) {
								println(elem2)
							}
						//}
					}
				}
			}
		}
		return 1
	}

	// private methods
	private def permutate[A] (list: List[A]): List[List[A]] = {
	    list match {
		   case List(elem) => List(List(elem))
		   case l =>
		    for {
		       i <- List.range(0, l.length)
		       p <- permutate(l.slice(0, i) ++ l.slice(i + 1, l.length))
		    } yield l(i) :: p
		}
	}

	private def filterBy[A] (list: List[A], f: (A) => Boolean): List[A] = {
		return list.filter(f(_))
	}

	private def sortBy[A] (list: List[A], s: (A, A) => Boolean): List[A] = {
		return list.sortWith(s(_, _))
	}

	private def init(numRows: Int, numCols: Int) (f: (Int, Int) => T): Matrix = (
 		for(i <- 1 to numRows) yield
 			(for(j <- 1 to numCols) yield f(i, j)).to[List]
 	).to[List]

	private def multiplyRows(row1: Row, row2: Row): T = {
		requireEquals(row1.length, row2.length)
		row1.zip(row2).map { t: (T, T) => t._1 * t._2 }.reduceLeft(_ + _)
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
    	row1.zip(row2).map { t: (T, T) =>
    		t._1 + t._2
    	}
	}

	private def add(matrix1: Matrix, matrix2: Matrix): Unit = {
		matrix1.zip(matrix2).map{ rows: (Row, Row) =>
		    rows._1.zip( rows._2 ).map { items: (T, T) =>
		      	items._1 + items._2
		    }
	  	}
	}

	private def substractRows(row1: Row, row2: Row): Unit = {
		requireEquals(row1.length, row2.length)
    	row1.zip(row2).map { t: (T, T) =>
    		t._1 - t._2
    	}
	}

	private def substract(matrix1: Matrix, matrix2: Matrix): Unit = {
		matrix1.zip(matrix2).map { rows: (Row, Row) =>
		    rows._1.zip( rows._2 ).map { items: (T, T) =>
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
    	//var (row1, row2) = (this.matrix(i1).updated(j1, temp2), this.matrix(i2).updated(j2, temp1))
		//this.matrix = this.matrix.updated(i1, row1)
		//this.matrix = this.matrix.updated(i2, row2)
	}

    private def swapByValue[T] (elem1: T, elem2: T, list: List[T]): List[T] = {
    	list map {
			case item if item == elem1 => elem2
    		case item if item == elem2 => elem1
    		//case item: List[T] => swap(elem1, elem2, item)
    		case item => item
		}
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
 	def filter(shapes: List[T]) (f: (T) => Boolean): List[T]
}

class Rectangle[T >: Int <: Int] (
	private var leftBottom: T,
	private var leftTop: T,
	private var rightTop: T,
	private var rightBottom: T) extends Shape[Rectangle[T]] { //extends Comparable[Rectangle];

	private val uuid: String = java.util.UUID.randomUUID.toString

	def this() = this(0, 0, 0, 0)
	def lBottom: T = leftBottom
	def lTop: T = leftTop
	def rTop: T = rightTop
	def rBottom: T = rightBottom
	def ID: String = uuid
 	override def *(rectangle2: Rectangle[T]) = multiply(this, rectangle2)
 	override def *(value: Int) = multiply(this, value)
 	override def +(rectangle2: Rectangle[T]) = add(this, rectangle2)
 	override def -(rectangle2: Rectangle[T]) = substract(this, rectangle2)
	override def sum: T = {
		return (this.lBottom + this.lTop + this.rTop + this.rBottom)
	}
	override def filter(rectangles: List[Rectangle[T]]) (f: (Rectangle[T]) => Boolean): List[Rectangle[T]] = (
		for (rectangle <- rectangles; if f(rectangle))
			yield rectangle
	).to[List]

	private def multiply(rectangle2: Rectangle[T]): Unit = {
		this.leftBottom *= rectangle2.lBottom
		this.leftTop *= rectangle2.lTop
		this.rightTop *= rectangle2.rTop
		this.rightBottom *= rectangle2.rBottom
	}

	private def multiply(value: Int): Unit = {
		this.leftBottom *= value
		this.leftTop *= value
		this.rightTop *= value
		this.rightBottom *= value
	}

	private def add(rectangle2: Rectangle[T]): Unit = {
		this.leftBottom += rectangle2.lBottom
		this.leftTop += rectangle2.lTop
		this.rightTop += rectangle2.rTop
		this.rightBottom += rectangle2.rBottom
	}

 	private def substract(rectangle2: Rectangle[T]): Unit = {
		this.leftBottom -= rectangle2.lBottom
		this.leftTop -= rectangle2.lTop
		this.rightTop -= rectangle2.rTop
		this.rightBottom -= rectangle2.rBottom
 	}

	private def multiply(rectangle1: Rectangle[T], rectangle2: Rectangle[T]): Rectangle[T] = {
		new Rectangle[T](rectangle1.lBottom * rectangle2.lBottom,
					  rectangle1.lTop * rectangle2.lTop,
					  rectangle1.rTop * rectangle2.rTop,
					  rectangle1.rBottom * rectangle2.rBottom
					 )
	}

	private def multiply(rectangle1: Rectangle[T], value: Int): Rectangle[T] = {
		new Rectangle[T](rectangle1.lBottom * value,
					  rectangle1.lTop * value,
					  rectangle1.rTop * value,
					  rectangle1.rBottom * value
					 )
	}

	private def add(rectangle1: Rectangle[T], rectangle2: Rectangle[T]): Rectangle[T] = {
		new Rectangle[T](rectangle1.lBottom + rectangle2.lBottom,
					  rectangle1.lTop + rectangle2.lTop,
					  rectangle1.rTop + rectangle2.rTop,
					  rectangle1.rBottom + rectangle2.rBottom
					 )
	}

 	private def substract(rectangle1: Rectangle[T], rectangle2: Rectangle[T]): Rectangle[T] = {
		new Rectangle[T](rectangle1.lBottom - rectangle2.lBottom,
					  rectangle1.lTop - rectangle2.lTop,
					  rectangle1.rTop - rectangle2.rTop,
					  rectangle1.rBottom - rectangle2.rBottom
					 )
 	}
	// override def compareTo(rectangle: Rectangle[T]) = {
	//     val lBottom = this.leftBottom compareTo rectangle.leftBottom
	//     if (lBottom != 0) lBottom
	//     else this.leftTop compareTo rectangle.leftTop
 	// }
	override def toString = s"{ID: ($ID), leftBottom: ($lBottom), leftTop: ($lTop), rightTop: ($rTop), rightBottom: ($rBottom)}"
}

object Rectangle {
	def apply(leftBottom: Int, leftTop: Int, rightTop: Int, rightBottom: Int) = init(leftBottom, leftTop, rightTop, rightBottom)
	def init(leftBottom: Int, leftTop: Int, rightTop: Int, rightBottom: Int) = new Rectangle[Int](leftBottom, leftTop, rightTop, rightBottom)
}

abstract class ShapeMask[T <: ShapeMask[T]] {
	// def sum(): Int
	// def *(shape2: T): T
	// def *(value: Int): T
	// def +(shape2: T): T
 // 	def -(shape2: T): T
 // 	def filter(shapes: List[T]) (f: (T) => Boolean): List[T]
}

class RectangleMask[T >: Null <: Rectangle[Int]] (
	private var leftBottom: T,
	private var leftTop: T,
	private var rightTop: T,
	private var rightBottom: T) extends ShapeMask[RectangleMask[T]] {

	def this() = this(null, null, null, null)
	def lBottom: T = leftBottom
	def lTop: T = leftTop
	def rTop: T = rightTop
	def rBottom: T = rightBottom

	def left: List[String] = List(leftBottom.ID, leftTop.ID)
	def top: List[String] = List(leftTop.ID, rightTop.ID)
	def right: List[String] = List(rightBottom.ID, rightTop.ID)
	def bottom: List[String] = List(leftBottom.ID, rightBottom.ID)

	override def toString = s"{leftBottom: ($lBottom), leftTop: ($lTop), rightTop: ($rTop), rightBottom: ($rBottom)}"
}