package annotation

import scala.annotation.StaticAnnotation

/**
  * Created by xiaziyao on 2017/4/20.
  */
case class Field(name: String, _type: String, annotations: List[StaticAnnotation]) {
  def getAnnotation[T <: StaticAnnotation](annClass: Class[T]) = {
    annotations.find(_.getClass.getName == annClass.getName)
  }
}
