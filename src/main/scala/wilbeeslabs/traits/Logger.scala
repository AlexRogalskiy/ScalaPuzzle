package wildbeeslabs.traits

import java.util.Calendar
import java.text.SimpleDateFormat

trait Logger {
	protected val defaultDateTimeFormat = "yyyy-MM-dd HH:mm:ss.SSS"

	def info(message: String) {
		print(s"[info] [${currentDateTime()}] $message")
	}

	def warn(message: String) {
		print(s"[warn] [${currentDateTime()}] $message")
	}

	def error(message: String) = {
		print(s"[error] [${currentDateTime()}] $message")
		cleanup()
		sys.exit(1)
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