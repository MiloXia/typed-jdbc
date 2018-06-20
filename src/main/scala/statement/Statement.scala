package statement
//
import java.sql.PreparedStatement
import dbtype._
import parameter.Parameter
import shapeless.labelled.FieldType
import shapeless.ops.hlist.{FillWith, IsHCons, Mapper}
import shapeless.{::, Generic, HList, HNil, LabelledGeneric, Poly, Poly1, Poly2, Witness}

import scala.annotation.implicitNotFound
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
  * Created by xiaziyao on 2017/4/12.
  */
class Statement[T](
  val sql: StringBuilder,
  val value: T, //case class
  val statType: StatementType.Value = StatementType.Sql, //TODO AutoId
  val autoId: Boolean = false
  ) {
  require(value != null)
  import Statement._

//  var parameters: HList = HNil
  val (parsedSql, parametersIndexes) = parseSql

  private def parseSql: (StringBuilder, mutable.HashMap[String, ListBuffer[Int]]) = {
//    val parameters = new ParameterCollection //TODO
    val parametersIndexes = new mutable.HashMap[String, ListBuffer[Int]]()
    var index = 1
    val s = sql.toString
    val parsedSql = new StringBuilder()
    var start = -1
    var n = 0
    var i = 0
    val length = s.length
    val end = length - 1
    while (i < length) {
      var c = s.charAt(i)
      if(c == separate) start = i + 1
      else if(start > 0 && (!Character.isJavaIdentifierPart(c) || i == end)) {
        val paramName = if(i == end) s.substring(start, i + 1) else s.substring(start, i)
        parsedSql.append(s.substring(n, start - 1)).append("?")
        n = i
        start = -1 //reset

        val parameterIndexes = parametersIndexes.get(paramName) match {
          case Some(indexes) => indexes
          case None =>
            val indexes = new ListBuffer[Int]()
            parametersIndexes += paramName -> indexes
            indexes
        }
        parameterIndexes += index
        index += 1
      }
      i += 1
    }

    if(n < end || ! Character.isJavaIdentifierPart(s.charAt(n))) {
      parsedSql.append(s.substring(n))
    }
    (parsedSql, parametersIndexes)
  }

  def mkParameters[R <: HList](implicit gen: LabelledGeneric.Aux[T, R], ps: Parameters[R]): ps.Out = {
    //create parameters
    Parameters.make(value, parametersIndexes)(gen, ps)
    //need filter ?
  }

  def mkParameters2[R <: HList](implicit gen: Generic.Aux[T, R], ps: Parameters[R]): ps.Out = {
    //create parameters
    Parameters.make2(value, parametersIndexes)(gen, ps)
    //need filter ?
  }

  def mkParameters3(implicit ps: Parameters[(T :: HNil)]): ps.Out = {
    //create parameters
    Parameters.make3(value, parametersIndexes)(ps)
    //need filter ?
  }
}
object Statement {
  val separate = '#'
}
//
object TestStmt {
  object size extends Poly1 {
//    implicit def default[T] = at[T](_ => 1)
    implicit def caseInt = at[Int](_ => 1)
    implicit def caseString = at[String](_.length)
//    implicit def default[T] = at[List[T]](_.size)
//    implicit def caseListInt = at[List[Int]](_.sum)
    implicit def caseOption[T](implicit st : Case.Aux[T, Int]) = at[Option[T]](x => (x map(n => size(n))).getOrElse(0))
//    implicit def caseListStr = at[List[String]](_.size)
  }
  //test
  class Ann(val name: String, val id: Int) extends scala.annotation.StaticAnnotation

  @Ann("hello2", 1)
  case class User(id: Option[Int], name: String, age: Int)
  case class A(i: Int)
//  case class A25(
//                  i1 : Int,
//                  i2 : Int,
//                  i3 : Int,
//                  i4 : Int,
//                  i5 : Int,
//                  i6 : Int,
//                  i7 : Int,
//                  i8 : Int,
//                  i9 : Int,
//                  i10 : Int,
//                  i11 : Int,
//                  i12 : Int,
//                  i13 : Int,
//                  i14 : Int,
//                  i15 : Int,
//                  i16 : Int,
//                  i17 : Int,
//                  i18 : Int,
//                  i19 : Int,
//                  i20 : Int,
//                  i21 : Int,
//                  i22 : Int,
//                  i23 : Int,
//                  i24 : Int,
//                  i25 : Int
//                )

  def main(args: Array[String]): Unit = {
    val map = new mutable.HashMap[String, ListBuffer[Int]]()
    val indexes1 = new ListBuffer[Int]()
    indexes1 += 1
    val indexes2 = new ListBuffer[Int]()
    indexes2 += 2
    val indexes3 = new ListBuffer[Int]()
    indexes3 += 3
    indexes3 += 4
    map += "id" -> indexes1
    map += "name" -> indexes2
    map += "age" -> indexes3

    type Res = Parameter[Option[Int], IntType.PolyF] :: Parameter[String, Varchar.PolyF] :: Parameter[Int, IntType.PolyF] :: HNil

    val p: Res = Parameters.make(User(Option(1), "a", 1), map)
    println(p)

    val gen = LabelledGeneric[User]
    val p2 = Parameters[gen.Repr]
    val r: Res = p2.parameters(gen.to(User(None, "a", 1)), map)
    implicitly[Res =:= p2.Out]
    println(r)
//    val p2 = Parameters(A25(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25))
//    println(p2)

    //DataSource do
//    val sql = new StringBuffer("select * from user where id = #id, age = #age, name = #name")
//    val stat = new Statement(sql, User(1, "a", 1), StatementType.Sql)
//    val newSql = stat.parsedSql
//    val pars: Res = stat.mkParameters
//    println(newSql, pars)
//    val res = pars.foldLeft(new PreparedStatement1)(prepareParameter2)
//    println(res)
    println(size(Option("123")))
  }
}

//object prepareParameter extends Poly1 {
//  implicit def default[T, P <: Poly1] = at[Parameter[T, P]](p => throw new IllegalArgumentException(s"can't prepare $p"))
//  implicit def caseP1: Case.Aux[Parameter[Int, IntType.PolyF], String] = at(_.name)
//  implicit def caseP11: Case.Aux[Parameter[Integer, IntType.PolyF], String] = at(_.name)
//  implicit def caseP2: Case.Aux[Parameter[String, Varchar.PolyF], String] = at(_.name)
//  implicit def caseP3: Case.Aux[Parameter[Byte, ByteType.PolyF], String] = at(_.name)
//  implicit def caseP4: Case.Aux[Parameter[AnyRef, UnknownType.PolyF], String] = at(_.name)
//}

//eq getFieldType
//object prepareParameter2 extends Poly2 {
//  implicit def caseP1: Case.Aux[PreparedStatement1, Parameter[Int, IntType.PolyF], PreparedStatement1] =
//    at((pstmt , param) => param.prepare2(pstmt))
//  implicit def caseP5: Case.Aux[PreparedStatement1, Parameter[Integer, IntType.PolyF], PreparedStatement1] =
//    at((pstmt , param) => param.prepare2(pstmt))
//  implicit def caseP2: Case.Aux[PreparedStatement1, Parameter[String, Varchar.PolyF], PreparedStatement1] =
//    at((pstmt , param) => param.prepare2(pstmt))
//  implicit def caseP3: Case.Aux[PreparedStatement1, Parameter[Byte, ByteType.PolyF], PreparedStatement1] =
//    at((pstmt , param) => param.prepare2(pstmt))
//  implicit def caseP4: Case.Aux[PreparedStatement1, Parameter[AnyRef, UnknownType.PolyF], PreparedStatement1] =
//    at((pstmt , param) => param.prepare2(pstmt))
//  implicit def default[T, P <: Poly1]: Case.Aux[PreparedStatement1, Parameter[T, P], PreparedStatement1] =
//    at((_, param) => throw new IllegalArgumentException(s"can't prepare $param"))
//}