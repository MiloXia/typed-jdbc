package dbtype
import java.sql.{PreparedStatement, Types}

import shapeless.{::, HList, HNil, Poly1}
import shapeless.ops.hlist.IsHCons.Aux
import shapeless.ops.hlist.{IsHCons, Mapper}
import shapeless.ops.hlist.Mapper.Aux

/**
  * Created by Admin on 2017/4/26.
  */
//java.math.BigDecimal
object DecimalType extends DBType {
  override val typeName: String = "decimal"
  override val sqlType: Int = Types.DECIMAL
  override type PolyF = setBigDecimal.type

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

  object setBigDecimal extends Poly1 {
    implicit val BigDecimalCase: Case.Aux[(PreparedStatement, Int, java.math.BigDecimal), PreparedStatement] =
      at({case (pstmt, index, v) => pstmt.setBigDecimal(index, v); pstmt})
    implicit val numberCase: Case.Aux[(PreparedStatement, Int, Number), PreparedStatement] =
      at({case (pstmt, index, v) => pstmt.setBigDecimal(index, new java.math.BigDecimal(v.toString)); pstmt})
    implicit val stringCase: Case.Aux[(PreparedStatement, Int, String), PreparedStatement] =
      at({case (pstmt, index, v) => pstmt.setBigDecimal(index, if(v.trim.isEmpty) new java.math.BigDecimal("0") else new java.math.BigDecimal(v)); pstmt})
  }

}
