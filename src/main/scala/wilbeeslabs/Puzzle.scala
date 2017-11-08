package com.wildbeeslabs

//import BitOperations

object Puzzle extends AppInitializer {
	println(greeting)
	val a = Array(2, 3, 5, 7, 11) 
	val result = for (elem <- a if elem % 2 == 0) yield 2 * elem
	for (elem <- result)  println(elem)
	val positionsToRemove = for (i <- a.indices if a(i) < 0) yield i 

	val triangle = new Array[Array[Int]](12)
	for (i <- triangle.indices)
		triangle(i) = new Array[Int](i + 1)


	val matrixDim = getDimension(7)
	var rectMatrix = Matrix.init(matrixDim, matrixDim)
	println(rectMatrix)

	val A = Matrix(5, 5) { (i:Int,j:Int) =>
 		if(i == j) 1 else 0
	}
	println(A)
}

trait AppInitializer {
	lazy val greeting: String = "hello"

	def loadDataFromFile(file: java.io.File): List[Rectangle] = ???

	def main(args: Array[String]) {
    	println("Initializing data set...")
    	for (arg <- args if arg startsWith "-") println(" " + (arg substring 1))
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

class Matrix(numRows: Int, numCols: Int) {
	private var matrix = Array.ofDim[Shape](numRows, numCols)
	//private var matrix: Option[Array[Array[Shape]]] = None
	
	def fill(shapes: List[Shape]): Unit = matrix match {
		case matrix: Array[Array[Shape]] =>
			for (i <- 1 to numRows) {
				for (j <- 1 to numCols) {
					matrix(i)(j) = null
				}
			}
  	}

	def getSumOfSquares: Int = matrix match {
		case matrix: Array[Array[Shape]] =>
		    1
	}

	override def toString: String = {
		var str: String = ""
		for (row <- matrix) {
			for (elem <- row) {
				str += elem + "\t"
			}
			str += "\n"
		}
		return str
	}
}

object Matrix {
	def apply(numRows: Int, numCols: Int) (f:(Int,Int) => Int) = (
 		for(i <- 1 to numRows) yield (for(j <- 1 to numCols) yield f(i,j)).toList
 	).toList

	def init(numRows: Int, numCols: Int) = new Matrix(numRows, numCols)
}

abstract class Shape {
	def sum(): Int
	//def sum(rectangles: List[Rectangle]): Int = {  var result: Int = 0; for (rectangle <- rectangles) result += rectangle.leftBottom;  result } 
}


class Rectangle (
	val leftBottom: Int,
	val	leftTop: Int,
	val	rightTop: Int,
	val	rightBottom: Int) extends Shape { //extends Comparable[Rectangle];

	override def toString = s"{leftBottom: ($leftBottom), leftTop: ($leftTop), rightTop: ($rightTop), rightBottom: ($rightBottom)}"

	def this() = this(0, 0, 0, 0)

	def sum(): Int = {  var result: Int = 0; result += leftBottom;  result } 
	// override def compareTo(rectangle: Rectangle) = {
	//     val lBottom = this.leftBottom compareTo rectangle.leftBottom
	//     if (lBottom != 0) lBottom
	//     else this.leftTop compareTo rectangle.leftTop
 //  	}

  	def getShapeList(rectangles: List[Rectangle]) = {
		for (rectangle <- rectangles; if rectangle.leftBottom < 18)
			yield rectangle.leftBottom
	}
}