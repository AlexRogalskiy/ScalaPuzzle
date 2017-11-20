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
package wildbeeslabs.classes

import wildbeeslabs.classes.CMatrix._

/**
 *
 * Abstract Shape Mask class
 *
 * @author Alex
 * @version 1.0.0
 * @since 2017-11-16
 * param <T>
 * param <S>
 */
abstract class ShapeMask[T <: ShapeMask[T, S], S >: Null <: Any] {
 	def diff(shapeMask2: T, f: (S) => Boolean = (S) => true): Set[S]
 	def intersect(shapeMask2: T, f: (S) => Boolean = (S) => true): Set[S]
 	def getShapeID[A >: Null <: Shape[A, B], B: Numeric] (rectShape: A): S = {
		rectShape match {
			case null => null
			case rect: Shape[A, B] => rect.ID.asInstanceOf[S]
		}
 	}
	def toMatrix: CMatrix[_, _]

 	protected def tuple2ToSet[A] (t: (A, A)): Set[A] 		= Set(t._1, t._2)
 	protected def tuple3ToSet[A] (t: (A, A, A)): Set[A] 	= Set(t._1, t._2, t._3)
	protected def tuple4ToSet[A] (t: (A, A, A, A)): Set[A] 	= Set(t._1, t._2, t._3, t._4)

	protected def tuple2ToList[A] (t: (A, A)): List[A] 			= List(t._1, t._2)
 	protected def tuple3ToList[A] (t: (A, A, A)): List[A] 		= List(t._1, t._2, t._3)
	protected def tuple4ToList[A] (t: (A, A, A, A)): List[A] 	= List(t._1, t._2, t._3, t._4)

	protected def intersectMask[A] (elemBorders: Set[A], elem2Borders: Set[A]): Set[A] = elemBorders.intersect(elem2Borders)
}