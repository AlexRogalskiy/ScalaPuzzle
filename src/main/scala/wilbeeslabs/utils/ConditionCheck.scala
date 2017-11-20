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
 * Condition Check class
 *
 * @author Alex
 * @version 1.0.0
 * @since 2017-11-16
 */
class ConditionCheck {
	def requireEquals[T] (a: => T, b: => T, checkPreconditions: Boolean = true): Unit = {
		if(checkPreconditions) {
			val a_, b_ = (a, b)
			if(a_ != b_) {
				throw new AssertionError("Precondition failed: " + a_ + " != " + b_)
			}
		}
	}
  
	def ensure[T] (postcondition: (T) => Boolean, result: T, checkPostconditions: Boolean = true): T = {
		if(checkPostconditions) {
			if(!postcondition(result)) {
				throw new AssertionError("Postcondition failed for result = " + result)
			}
		}
		return result
	}
}

object ConditionCheck extends ConditionCheck {

	private val checkPreconditions: Boolean = false
	private val checkPostconditions: Boolean = false

	def requireEquals[T] (a: => T, b: => T): Unit = requireEquals[T] (a, b, checkPreconditions)
	def ensure[T] (postcondition: (T) => Boolean, result: T): T = ensure[T] (postcondition, result, checkPostconditions)
}