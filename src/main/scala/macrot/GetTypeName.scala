package macrot

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

/**
  * Created by xiaziyao on 2017/5/23.
  */
object GetTypeName {
  def typeName[T]: String = macro typeNameImpl[T]

  def typeNameImpl[T: c.WeakTypeTag](c: blackbox.Context): c.Expr[String] = {
    import c.universe._
    val code = q"""${weakTypeOf[T].toString}"""
    c.Expr[String](code)
  }
}

object RecordSyntax { //just rename
  val key = shapeless.syntax.singleton

  case class Record[T](name: String, rec: T) {
    def tableName = {
      val _name = name.split("\\.")
      if(_name.nonEmpty) {
        _name.last
      } else name
    }
  }
  trait Temp {
    def apply[T](rec: T): Record[T]
  }

  def vo[A]: RecordSyntax.Temp = macro recordImpl[A]
  def record[A]: RecordSyntax.Temp = macro recordImpl[A]

  def recordImpl[T: c.WeakTypeTag](c: blackbox.Context): c.Expr[RecordSyntax.Temp] = {
    import c.universe._
    val code =
      q"""
         new macrot.RecordSyntax.Temp {
            def apply[T](rec: T) = new macrot.RecordSyntax.Record[T](${weakTypeOf[T].toString}, rec)
         }
       """
    c.Expr[RecordSyntax.Temp](code)
  }
}