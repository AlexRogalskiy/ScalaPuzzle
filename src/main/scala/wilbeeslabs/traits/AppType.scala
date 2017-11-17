package wildbeeslabs.traits

import wildbeeslabs.classes.Rectangle
import wildbeeslabs.classes.RectangleMask
import wildbeeslabs.classes.Rectangle4Mask

trait AppType {
	type OptionMap 		= Map[Symbol, Any]
	type RectInt 		= Rectangle[Int]
	type RectIntMask 	= RectangleMask[RectInt]
	type RectIntMask4 	= Rectangle4Mask[RectIntMask]

	type RectTupleOf2 	= Tuple2[RectInt, RectInt]
	type RectTupleOf3 	= Tuple3[RectInt, RectInt, RectInt]
	type RectTupleOf4 	= Tuple4[RectInt, RectInt, RectInt, RectInt]
}