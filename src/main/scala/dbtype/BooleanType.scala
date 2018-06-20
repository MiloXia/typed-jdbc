package dbtype
import java.sql.Types

import java.sql.PreparedStatement
import shapeless.{::, HList, HNil, Poly1}
import shapeless.ops.hlist.IsHCons.Aux
import shapeless.ops.hlist.{IsHCons, Mapper}
import shapeless.ops.hlist.Mapper.Aux

/**
  * Created by Admin on 2017/4/25.
  */
object BooleanType extends DBType {
  override val typeName: String = "boolean"
  override val sqlType: Int = Types.BOOLEAN
  override type PolyF = setBoolean.type

  override def setParameter[T](pstmt: PreparedStatement, index: Int, value: T): PreparedStatement = ???

  override def setParameter2[T, O <: HList, PolyF2 <: Poly1](pstmt: PreparedStatement, index: Int, value: T)
                                                            (implicit mapper: Mapper.Aux[PolyF2, (PreparedStatement, Int, T) :: HNil, O],
                                                             isCons: IsHCons.Aux[O, PreparedStatement, HNil]): PreparedStatement = {
    if (value == null) {
      pstmt.setNull(index, sqlType)
      pstmt
    } else {
      mapper((pstmt, index, value) :: HNil)
      pstmt
    }
  }

  object setBoolean extends Poly1 {
    implicit val booleanCase: Case.Aux[(PreparedStatement, Int, Boolean), PreparedStatement] =
      at({case (pstmt, index, v) => pstmt.setBoolean(index, v); pstmt})
    implicit val stringCase: Case.Aux[(PreparedStatement, Int, String), PreparedStatement] =
      at({case (pstmt, index, v) => pstmt.setBoolean(index, if(v.trim == "1") true else if(v.trim == "0" || v.trim.isEmpty) false else v.toBoolean); pstmt})
    implicit val BooleanCase: Case.Aux[(PreparedStatement, Int, java.lang.Boolean), PreparedStatement] =
      at({case (pstmt, index, v) => pstmt.setBoolean(index, v.booleanValue()); pstmt})
  }


}
