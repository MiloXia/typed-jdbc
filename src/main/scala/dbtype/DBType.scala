package dbtype

//import java.sql.PreparedStatement

import java.sql.PreparedStatement
import shapeless.{::, HList, HNil, Poly1}
import shapeless.ops.hlist.{IsHCons, Mapper}

import scala.annotation.{implicitNotFound, switch}

/**
  * Created by xiaziyao on 2017/4/12.
  */
trait DBType {
  val typeName: String
  val sqlType: Int
  type PolyF <: Poly1
  def setParameter[T](pstmt: PreparedStatement, index: Int, value: T): PreparedStatement

  def setParameter2[T, O <: HList, PolyF2 <: Poly1](pstmt: PreparedStatement, index: Int, value: T)
                                  (implicit mapper: Mapper.Aux[PolyF2, (PreparedStatement, Int, T) :: HNil, O],
                                   isCons: IsHCons.Aux[O, PreparedStatement, HNil]): PreparedStatement

  def setParameter3[T, O <: HList](pstmt: PreparedStatement, index: Int, value: T)
                                  (implicit mapper: Mapper.Aux[PolyF, (PreparedStatement, Int, T) :: HNil, O],
                                   isCons: IsHCons.Aux[O, PreparedStatement, HNil]): PreparedStatement = {
    setParameter2[T, O, this.PolyF](pstmt, index, value)(mapper, isCons)
  }
}

object DBType {
  def apply(sqlType: Int): DBType = (sqlType: @switch) match {
    case ByteType.sqlType => ByteType
    //TODO mapping
    case _ => UnknownType
  }
}
