package wildbeeslabs.classes

import wildbeeslabs.utils.ConditionCheck._
import scala.collection.mutable.ListBuffer

class CMatrix[T >: Null <: Shape[T, S], S <: Any] (numRows: Int = 1, numCols: Int = 1) {
	/* Matrix type aliases */
	type Row = List[T]
	type Matrix = List[Row]

	private var matrix: Matrix = init(numRows, numCols) { (i: Int, j: Int) => { null } }

	/* public methods */
	def apply(i: Int, j: Int) 	= this.matrix(i)(j)
	def this() 					= this(0, 0)
	def T: Unit 				= transpose(this.matrix)
	def rowCount: Int 			= this.matrix.length
 	def colCount: Int 			= this.matrix.head.length
 	def size: Int 				= rowCount * colCount
 	def isSquare: Boolean 		= (rowCount == colCount)

 	def minor(i: Int, j: Int): Unit = minorMatrix(matrix, i, j)
 	def beside(matrix2: CMatrix[T, S]): Unit = this.matrix.zip( matrix2.matrix ).map{ t:(Row, Row) => t._1 ::: t._2 }
  	def above(matrix2: CMatrix[T, S]): Unit = this.matrix ::: matrix2.matrix
  	
  	def filterRows(filter: (Row) => Boolean): Matrix = filterBy[Row](this.matrix, filter)
  	def sortRows(sort: (Row, Row) => Boolean): Matrix = sortBy[Row](this.matrix, sort)
  	
  	def permutate(rowFilter: (Row) => Boolean = (Row) => true, elemPreFilter: (T) => Boolean = (T) => true, elemPostFilter: (Row) => Boolean = (Row) => true): Matrix = permutateMatrix(this.matrix, rowFilter, elemPreFilter, elemPostFilter)
  	def combinate(size: Int, rowFilter: (Row) => Boolean = (Row) => true, elemPreFilter: (T) => Boolean = (T) => true, elemPostFilter: (Row) => Boolean = (Row) => true): Matrix = combinateMatrix(this.matrix, size, rowFilter, elemPreFilter, elemPostFilter)
 	
	def *(value: S): Unit 					= multiply(this.matrix, value)
 	def *(matrix2: CMatrix[T, S]): Unit 	= multiplyMatrices(this.matrix, matrix2.matrix)
 	def +(matrix2: CMatrix[T, S]): Unit 	= add(this.matrix, matrix2.matrix)
 	def -(matrix2: CMatrix[T, S]): Unit 	= substract(this.matrix, matrix2.matrix)

 	def update(matrix2: Matrix): Unit 	= this.matrix = matrix2
 	def fill(value: T): Unit = this.foreach( (t: T) => { value } )
	def fill(f: (Int, Int) => T): Unit = this.matrix = this.init(rowCount, colCount) { f(_, _) }
	def fill(list: List[T], placeHolder: T = null): Unit = {
		def insertPlaceHolder(buffer: ListBuffer[T], delta: Int, value: T): Unit = {
			var c = 0
			while(c < delta) {
				buffer += value;
				c += 1
			}
		}
		var result = new ListBuffer[T]()
		if (this.size > list.length) {
			var delta = (this.size - list.length + 1) / 2
			insertPlaceHolder(result, delta / 2, placeHolder)
			var (left, right) = list.splitAt(colCount - delta)
			left.map(result += _)
			insertPlaceHolder(result, (delta - delta / 2), placeHolder)
			var last = (right.length % colCount)
			var (left_, right_) = right.splitAt(right.length - last)
			left_.map(result += _)
			insertPlaceHolder(result, (this.size - list.length - delta) / 2, placeHolder)
			right_.map(result += _)
			insertPlaceHolder(result, (this.size - list.length - delta) / 2, placeHolder)
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
	private def init(numRows: Int, numCols: Int) (f: (Int, Int) => T): Matrix = {
		(for(i <- 1 to numRows) yield
 			(for(j <- 1 to numCols) yield f(i, j)).to[List]).to[List]
	}
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

	def toStringFormat(colDelim: String = " ", rowDelim: String = "\n") (f: (T) => Boolean = (T) => true): String = {
		return "\n" + this.matrix.map(_.filter(f(_)).map(_.toStringFormat(colDelim) + rowDelim).reduceLeft(_ + _)).reduceLeft(_ + _)
	}
	override def toString = "\n" + this.matrix.map {_.map{_ + " "}.reduceLeft(_ + _)}.reduceLeft(_ + _) + "\n"
}

object CMatrix {
	implicit def apply[T >: Null <: Shape[T, S], S <: Any](numRows: Int, numCols: Int): CMatrix[T, S] = new CMatrix[T, S](numRows, numCols)
}