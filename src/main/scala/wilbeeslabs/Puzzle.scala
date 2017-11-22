/*
 * The MIT License
 *
 * Copyright 2017 WildBees Labs.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */
package wildbeeslabs

import wildbeeslabs.traits.AppType
import wildbeeslabs.classes.Rectangle4Mask
import wildbeeslabs.classes.RectangleMask
import wildbeeslabs.classes.Rectangle
import wildbeeslabs.classes.CMatrix

/**
 *
 * Application entrypoint loader
 *
 * @author Alex
 * @version 1.0.0
 * @since 2017-11-16
 */
object Puzzle extends AppInitializer {
	/* Default program usage information */
	val usage =
	"""
	>>> Program options information <<<

		Usage: run --file filename

	>>> Copyright by wildbeeslabs (с) <<<
	"""
	def main(args: Array[String]): Unit = {
    	if (args.length == 0) {
	        println(usage)
	        return
	    }
    	val options = nextOption(Map().withDefaultValue("not found"), args.to[List])
    	runPuzzleTask(options)
    }
}

/**
 *
 * Application Initializer trait
 *
 * @author Alex
 * @version 1.0.0
 * @since 2017-11-16
 */
trait AppInitializer extends AppType {
	import scala.io.Codec
	import scala.io.Source
	import scala.io.BufferedSource
	import scala.collection.mutable.ListBuffer

	implicit val decoder = Codec.UTF8.decoder
	decoder.onMalformedInput(java.nio.charset.CodingErrorAction.IGNORE)
	decoder.onUnmappableCharacter(java.nio.charset.CodingErrorAction.IGNORE)

	private val defaultFileRecordFormat = "\\d{1}\\s+\\d{1}\\s+\\d{1}\\s+\\d{1}"
	private val defaultFileName = "default.txt"

	def runPuzzleTask(optionMap: OptionMap): Unit = {
		val combinationGroupSize: Int = 4
		def loadRectangleData[A] (): Iterable[A] = {
			try {
				var inputDataList = loadDataFromFile[A] (
										optionMap.getOrElse('file, defaultFileName).toString,
										(str: String) => { str.trim.matches(defaultFileRecordFormat) },
										(str: String) => { str.split("\\s+").map((value) => toInt(value.trim).getOrElse(0)).toList.asInstanceOf[A] }
									)
				return inputDataList
			} catch {
				case ex: Exception => {
					println(s"\n\nERROR: cannot process load data operation: message = (${ex.getMessage()})\n\n")
					sys.exit(0)
				}
			}
			return null
		}
		def createRectangleMatrix[A <% List[Int]] (data: Iterable[A]): CMatrix[RectInt, Int] = {
			if(null == data || data.isEmpty) return null
			var rectangleList = data.map(elem => Rectangle(elem(2), elem(0), elem(1), elem(3))).to[List]
			val matrixDimension = getMaxPowerOf2(rectangleList.length)
			var resultSet = CMatrix[RectInt, Int](matrixDimension, matrixDimension)
			resultSet.fill(rectangleList, Rectangle.emptyRectangle)
			return resultSet
		}
		def generateRectangleCombinations(matrix: CMatrix[RectInt, Int]): Iterable[RectIntMask] = {
			var resultSet = new ListBuffer[RectIntMask]()
			if(null == matrix) {
				return resultSet
			}
			var rectangleCombinations = matrix.combine(combinationGroupSize, (r: List[RectInt]) => true, (r: RectInt) => (Rectangle.emptyRectangle != r))
			rectangleCombinations.map( elem => {
				elem.permutations.foreach {
					row => {
						var rectangleMask = RectangleMask(row(3), row(0), row(1), row(2))
						if(rectangleMask.validate(Rectangle.emptyRectangle)) {
							resultSet += rectangleMask
						}
					}
				}
			})
			return resultSet
		}
		def generateRectanglePermutations(list: Iterable[RectIntMask]): Iterable[RectIntMask4] = {
			/* Buffer for left / top / right / bottom sides of rectangle mask */
			var resultSet 	= new ListBuffer[RectIntMask4]()
			/* Buffers for left / top / right / bottom sides of rectangle */
			var buffLeft 	= new ListBuffer[RectIntMask]()
			var buffTop 	= new ListBuffer[RectIntMask]()
			var buffRight 	= new ListBuffer[RectIntMask]()
			var buffBottom 	= new ListBuffer[RectIntMask]()

			if(null == list || list.isEmpty) {
				return resultSet
			}

			def clearBuffers(): Unit = {
				buffLeft.clear()
				buffTop.clear()
				buffRight.clear()
				buffBottom.clear()
			}
			def isNotEmpty(buffer: ListBuffer[RectIntMask]): Boolean = {
				return !buffer.isEmpty
			}

			list.filter(!_.hasPlaceholder(Rectangle.emptyRectangle)).map(
				elem => {
					clearBuffers()
					list map {
						elem2 => {
						 	if (elem.leftBorder == elem2.rightBorder && elem2.intersectLeft(elem).isEmpty) 		buffLeft += elem2
							if (elem.rightBorder == elem2.leftBorder && elem2.intersectRight(elem).isEmpty) 	buffRight += elem2
							if (elem.topBorder == elem2.bottomBorder && elem2.intersectTop(elem).isEmpty) 		buffTop += elem2
							if (elem.bottomBorder == elem2.topBorder && elem2.intersectBottom(elem).isEmpty)	buffBottom += elem2
						}
					}
					if(isNotEmpty(buffLeft) && isNotEmpty(buffRight) && isNotEmpty(buffTop) && isNotEmpty(buffBottom)) {
						buffLeft.map( elemLeft => {
							buffRight.map( elemRight => {
								if(elemLeft.intersect(elemRight, (s: String) => s != Rectangle.emptyRectangle.ID).isEmpty) {
									buffTop.map( elemTop => {
										buffBottom.map( elemBottom => {
											if(elemTop.intersect(elemBottom, (s: String) => s != Rectangle.emptyRectangle.ID).isEmpty &&
												elemLeft.intersectLeft(elemTop).isEmpty &&
												elemTop.intersectTop(elemRight).isEmpty &&
												elemRight.intersectRight(elemBottom).isEmpty &&
												elemBottom.intersectBottom(elemLeft).isEmpty) {
												var rectangle4Mask = Rectangle4Mask(elemLeft, elemTop, elemRight, elemBottom)
												if(rectangle4Mask.validate(Rectangle.pruneRectangle)) {
													resultSet += rectangle4Mask
												}
											}
										})
									})
								}
							})
						})
					}
				}
			)
			return resultSet
		}
		def showRectangleResultSet(list: Iterable[RectIntMask4]): Unit = {
			if(null == list || list.isEmpty) {
				println("\nCannot find possible rectangle combinations, please check possible matrix options / input data set\n")
				return
			}
			list.map(elem => {
				println(elem.toMatrix[RectInt, Int].toStringFormat() { (r: RectInt) => (!Rectangle.typeList.contains(r)) })
			})
		}

		var rectangleData 	= loadRectangleData[List[Int]]()
		var rectangleMatrix = createRectangleMatrix[List[Int]](rectangleData)
		var rectCombiList 	= generateRectangleCombinations(rectangleMatrix)
		var rectPermList 	= generateRectanglePermutations(rectCombiList)
		showRectangleResultSet(rectPermList)
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

	def loadDataFromFile[A] (filename: String, predicat: (String) => Boolean, process: (String) => A): List[A] = {
		var result = new ListBuffer[A]()
		var file = new java.io.File(filename)
		if(!file.exists() || !file.isFile()) {
			throw new java.io.FileNotFoundException(s"Cannot find file specified: ${filename}")
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
    	var p = 0;
    	var m = n
    	while(m > 0) {
    		p += 1
    		m >>= 1
    	}
    	return p
    }
}