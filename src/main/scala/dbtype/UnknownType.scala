package dbtype

import java.sql.Types

import java.sql.PreparedStatement
import shapeless.{::, HList, HNil, Poly1}
import shapeless.ops.hlist.{IsHCons, Mapper}

import scala.annotation.implicitNotFound

/**
  * Created by xiaziyao on 2017/4/12.
  */
object UnknownType extends DBType {
  val typeName: String = "other" //not sql true|false|unknown type
  val sqlType: Int = Types.OTHER

  override def setParameter[T](pstmt: PreparedStatement, index: Int, value: T): PreparedStatement = ???

  override type PolyF = setObject.type

  override def setParameter2[T, O <: HList, PolyF2 <: Poly1](pstmt: PreparedStatement, index: Int, value: T)
                                           (implicit mapper: Mapper.Aux[PolyF2, (PreparedStatement, Int, T) :: HNil, O],
                                            isCons: IsHCons.Aux[O, PreparedStatement, HNil]): PreparedStatement = {
    if (value == null) {
      pstmt.setNull(index, sqlType)
      pstmt
    } else {
      val r = mapper((pstmt, index, value) :: HNil)
      r.head
//      pstmt
    }
  }

  object setObject extends Poly1 {
    implicit val intCase: Case.Aux[(PreparedStatement, Int, AnyRef), PreparedStatement] =
      at({case (pstmt, index, v) => pstmt.setObject(index, v); pstmt})
  }
}
