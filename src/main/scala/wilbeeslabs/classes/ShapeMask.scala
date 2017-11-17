package wildbeeslabs.classes

abstract class ShapeMask[T <: ShapeMask[T, S], S <: Any] {
 	def diff(shapeMask2: T, f: (S) => Boolean = (S) => true): Set[S]
 	def intersect(shapeMask2: T, f: (S) => Boolean = (S) => true): Set[S]
 	def getShapeID[A >: Null <: Shape[A, B], B <: Any](rectShape: A): String = {
 		if (null == rectShape)
 			return null
 		return rectShape.ID
 	}

 	protected def tuple2ToSet[A] (t: (A, A)): Set[A] 		= Set(t._1, t._2)
 	protected def tuple3ToSet[A] (t: (A, A, A)): Set[A] 	= Set(t._1, t._2, t._3)
	protected def tuple4ToSet[A] (t: (A, A, A, A)): Set[A] 	= Set(t._1, t._2, t._3, t._4)

	protected def tuple2ToList[A] (t: (A, A)): List[A] 			= List(t._1, t._2)
 	protected def tuple3ToList[A] (t: (A, A, A)): List[A] 		= List(t._1, t._2, t._3)
	protected def tuple4ToList[A] (t: (A, A, A, A)): List[A] 	= List(t._1, t._2, t._3, t._4)

	protected def intersectMask[A] (elemBorders: Set[A], elem2Borders: Set[A]): Set[A] = elemBorders.intersect(elem2Borders)
}