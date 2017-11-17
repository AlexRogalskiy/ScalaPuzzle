package wildbeeslabs.classes

abstract class Shape[T <: Shape[T, S], S] {
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