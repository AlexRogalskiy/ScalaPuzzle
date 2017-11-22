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

/**
 *
 * Abstract Shape class
 *
 * @author Alex
 * @version 1.0.0
 * @since 2017-11-16
 * @param <T>
 * @param <S>
 */
abstract class Shape[T <: Shape[T, S], S: Numeric] {
	protected val uuid: String = java.util.UUID.randomUUID.toString
	def ID: String 	= uuid
	def sum: S
	def *(value: S): T
	def *(shape2: T): T
	def +(shape2: T): T
 	def -(shape2: T): T
 	def filter(shapes: List[T]) (f: (T) => Boolean): List[T]
 	def toStringFormat(delim: String = " "): String
}
