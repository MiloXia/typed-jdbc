package parameter

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
  * Created by xiaziyao on 2017/4/12.
  */
//class ParameterCollection {
//  val keys = new ListBuffer[String]() //TODO Vec
//  val values = new mutable.HashMap[String, Parameter[_]]() //TODO type
//
//  def get(index: Int) = values.get(keys(index)) //TODO option
//  def get(name: String) = values.get(name)
//
//  def add(name: String, param: Parameter[_]) = {
//    require(name == param.name) //TODO
//    if(!values.contains(name)) {
//      keys += name
//    }
//    values += (name -> param)
//  }
//
//  def size = keys.size
//
//  //TODO other method
//}
