package com.wildbeeslabs.utils

object ConditionCheck {

  var checkPreconditions = false
  var checkPostconditions = false

  def requireEquals[T] (a: => T, b: => T): Unit = {
    if(checkPreconditions){
      val a_, b_ = (a,b)
      if(a_ != b_) {
	     throw new AssertionError("Precondition failed: " + a_ + " != " + b_)
      }
    }
  }
  
  def ensure[T] (postcondition: (T) => Boolean, result: T): Unit = {
    if(checkPostconditions) {
      if(!postcondition(result)) {
  	   throw new AssertionError("Postcondition failed for result = " + result)
      }
    }
  }
}