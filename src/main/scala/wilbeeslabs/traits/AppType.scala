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
package wildbeeslabs.traits

import wildbeeslabs.classes.Rectangle
import wildbeeslabs.classes.RectangleMask
import wildbeeslabs.classes.Rectangle4Mask

/**
 *
 * Application Types trait
 *
 * @author Alex
 * @version 1.0.0
 * @since 2017-11-16
 */
trait AppType {
	type OptionMap 		= Map[Symbol, Any]
	type RectInt 		= Rectangle[Int]
	type RectIntMask 	= RectangleMask[RectInt]
	type RectIntMask4 	= Rectangle4Mask[RectIntMask]

	type RectTupleOf2 	= Tuple2[RectInt, RectInt]
	type RectTupleOf3 	= Tuple3[RectInt, RectInt, RectInt]
	type RectTupleOf4 	= Tuple4[RectInt, RectInt, RectInt, RectInt]
}