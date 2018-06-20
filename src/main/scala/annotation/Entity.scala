package annotation

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context
import scala.annotation.compileTimeOnly
/**
  * Created by xiaziyao on 2017/4/18.
  */
@compileTimeOnly("enable macro paradise to expand macro annotations")
class Entity extends scala.annotation.StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro TableMacro.init
}
object TableMacro {
  def init(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
    val result = {
      annottees.map(_.tree).toList match {
//        case q"case class $name extends ..$parents { ..$body }" :: Nil =>
        case q"case class $name (..$args)" :: Nil =>
          val fields = args.map {
            case q"..$mods val $fieldName : $fieldType = $other" => (fieldName, fieldType, mods)
          }

          val fieldCodes = fields.map { case (fieldName, fieldType, mode) =>
            val anns = mode match {
              case Modifiers(_, _, annotations) => annotations
            }
            q"""
               annotation.Field(${fieldName.toString()}, ${fieldType.toString()}, $anns: List[scala.annotation.StaticAnnotation])
              """
          }
//          val fieldsCode = q""" List($fieldCodes) """
          val argsCode = fields.map { case (fieldName, fieldType, _) =>
            q"$fieldName : $fieldType"
          }
          val appArgsCode = fields.map { case (fieldName, fieldType, _) =>
            fieldName
          }
          q"""
            case class $name (..$args)
            object ${TermName(name.toString())} {
              import annotation._
              def apply2(..$argsCode): $name = {
                println("hello")
                new $name(..$appArgsCode)
              }
              val tableName: String = ${Literal(Constant(name.toString()))}
              val fields: List[Field] = $fieldCodes
              EntityMaps.put(tableName, Table(tableName, fields))
            }
          """
        case _ =>
          q"""
            object P {
              def hello = println("hello")
            }
          """
      }
    }
    c.Expr[Any](result)
  }
}
