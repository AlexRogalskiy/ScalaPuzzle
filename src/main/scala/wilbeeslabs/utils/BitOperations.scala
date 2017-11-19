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
package wildbeeslabs.utils

/**
 *
 * Bit Operations class
 *
 * @author Alex
 * @version 1.0.0
 * @since 2017-11-16
 */
class BitOperations {
	trait Int {
		type Int = scala.Int
		def zero(i: Int, mask: Int)                 = (i & mask) == 0
		def mask(i: Int, mask: Int)                 = i & (complement(mask - 1) ^ mask)
		def hasMatch(key: Int, prefix: Int, m: Int) = mask(key, m) == prefix
		def unsignedCompare(i: Int, j: Int)         = (i < j) ^ (i < 0) ^ (j < 0)
		def shorter(m1: Int, m2: Int)               = unsignedCompare(m2, m1)
		def complement(i: Int)                      = (-1) ^ i
		def bits(num: Int)                          = 31 to 0 by -1 map (i => (num >>> i & 1) != 0)
		def bitString(num: Int, sep: String = "")   = bits(num) map (b => if (b) "1" else "0") mkString sep

		def highestOneBit(j: Int) = {
		  var i = j
		  i |= (i >>  1)
		  i |= (i >>  2)
		  i |= (i >>  4)
		  i |= (i >>  8)
		  i |= (i >> 16)
		  i - (i >>> 1)
		}
  	}
  	object Int extends Int

  	trait Long {
		type Long = scala.Long
		def zero(i: Long, mask: Long)                  = (i & mask) == 0L
		def mask(i: Long, mask: Long)                  = i & (complement(mask - 1) ^ mask)
		def hasMatch(key: Long, prefix: Long, m: Long) = mask(key, m) == prefix
		def unsignedCompare(i: Long, j: Long)          = (i < j) ^ (i < 0L) ^ (j < 0L)
		def shorter(m1: Long, m2: Long)                = unsignedCompare(m2, m1)
		def complement(i: Long)                        = (-1L) ^ i
		def bits(num: Long)                            = 63L to 0L by -1L map (i => (num >>> i & 1L) != 0L)
		def bitString(num: Long, sep: String = "")     = bits(num) map (b => if (b) "1" else "0") mkString sep

		def highestOneBit(j: Long) = {
		  var i = j
		  i |= (i >>  1)
		  i |= (i >>  2)
		  i |= (i >>  4)
		  i |= (i >>  8)
		  i |= (i >> 16)
		  i |= (i >> 32)
		  i - (i >>> 1)
		}
  	}
  	object Long extends Long
}