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

import java.util.Calendar
import java.text.SimpleDateFormat

/**
 *
 * Logger trait
 *
 * @author Alex
 * @version 1.0.0
 * @since 2017-11-16
 */
trait Logger {
	protected val defaultDateTimeFormat = "yyyy-MM-dd HH:mm:ss.SSS"

	def info(message: String, filename: String = null) {
		val msg = s"[info] [${currentDateTime()}] $message"
		filename match {
			case null => print(msg)
			case fn: String => print(msg, fn)
		}
	}

	def warn(message: String, filename: String = null) {
		val msg = s"[warn] [${currentDateTime()}] $message"
		filename match {
			case null => print(msg)
			case fn: String => print(msg, fn)
		}
	}

	def error(message: String, filename: String = null) = {
		val msg = s"[error] [${currentDateTime()}] $message"
		filename match {
			case null => print(msg)
			case fn: String => print(msg, fn)
		}
		cleanup()
		sys.exit(1)
	}
	
	protected def print(message: String, filename: String) {
		this.print(message, new java.io.File(filename))
	}
	
	protected def print(message: String, file: java.io.File) {
		if (!file.exists() || !file.isFile()) {
			return
		}
		val printWriter = new java.io.PrintWriter(file)
		try { printWriter.println(message) } finally { printWriter.close() }
	}

	protected def print(message: String) {
		println(message)
	}

	protected def cleanup() { }

	protected def currentDateTime(dateTimeFormat: String = defaultDateTimeFormat): String = {
		val currentTime = Calendar.getInstance().getTime()
		val currentFormat = new SimpleDateFormat(dateTimeFormat)
		try {
			return currentFormat.format(currentTime)
		} catch {
			case ex: Throwable => ""
		}
	}
}