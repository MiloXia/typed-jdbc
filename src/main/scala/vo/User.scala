package vo

import annotation.{AutoId, Entity, Length}

/**
  * Created by xiaziyao on 2017/4/18.
  */
//@Entity
case class User(@AutoId id: Option[Int], @Length(12) name: String, age: Int)