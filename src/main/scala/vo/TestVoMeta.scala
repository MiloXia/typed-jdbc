package vo

import annotation.EntityMaps
import annotation.{AutoId, Length}
/**
  * Created by xiaziyao on 2017/4/18.
  */
object TestVoMeta {
  def main(args: Array[String]): Unit = {
    import scala.reflect.runtime.universe._
    println(show(reify{
      @annotation.Entity
      case class UserX(id: Option[Int], name: String, age: Int)
    }))
//    println(User.tableName, User.fields)
//    println(User.apply2(None, "a", 0))
//    val fields = EntityMaps.getEntity(classOf[User].getSimpleName).get.fields
//    fields.filter(_.annotations.nonEmpty).foreach { field =>
//      field.getAnnotation(classOf[Length]).foreach { ann =>
//        println(ann.asInstanceOf[Length].value)
//      }
//    }
  }
}
