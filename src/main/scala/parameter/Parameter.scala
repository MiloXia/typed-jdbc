package parameter

import java.sql.PreparedStatement

import dbtype._
import shapeless.{::, HList, HNil, Poly1}
import shapeless.ops.hlist.{IsHCons, Mapper}

/**
  * Created by xiaziyao on 2017/4/12.
  */
class Parameter[T, PolyF <: Poly1](
                    val name: String,
//                    val sqlType: Int,
                    val value: T,
                    val indexes: Seq[Int],
                    val dbType: DBType /*, val dbTypeOpt: Option[DBType] = None*/) {
//  val dbType = DBType(sqlType) //???? sqlType != dbType ????

  val sqlType = dbType.sqlType

  def prepare(pstmt: PreparedStatement): PreparedStatement = {
    indexes.foldLeft(pstmt)((pstmt, index) =>
      dbType.setParameter(pstmt, index, value)
    )
  }

  def prepare2[O <: HList](pstmt: PreparedStatement)
                          (implicit mapper: Mapper.Aux[PolyF, (PreparedStatement, Int, T) :: HNil, O],
                           isCons: IsHCons.Aux[O, PreparedStatement, HNil]
              ): PreparedStatement = {
    indexes.foldLeft(pstmt)((pstmt, index) =>
      {//println("index " + index + " value " + value)
      dbType.setParameter2[T, O, PolyF](pstmt, index, value)(mapper, isCons)}
    )
  }

  //TODO register

  override def toString = s"Parameter($name, $sqlType, $value, $indexes)"
}


object Parameter {
//  def main(args: Array[String]): Unit = {
//    val param = new Parameter[Integer, IntType.PolyF]("id", 1, List(1, 2), IntType)
//    val ps1 = param.prepare2(new PreparedStatement)
//    println(ps1)
//    val param2 = new Parameter[Byte, ByteType.PolyF]("id", 1.toByte, List(1, 2), ByteType)
//    val ps2 = param2.prepare2(new PreparedStatement)
//    println(ps2)
//    val param3 = new Parameter[AnyRef, UnknownType.PolyF]("id", 2.asInstanceOf[AnyRef], List(1, 2), UnknownType)
//    val ps3 = param3.prepare2(new PreparedStatement)
//    println(ps3)
//  }
}

object ParameterTypeMapping {
  type Int_IntType = Parameter[Int, IntType.PolyF]
  type String_Varchar = Parameter[String, Varchar.PolyF]
  //TODO add more
}