package dbtype
import java.sql.Types

import java.sql.PreparedStatement
import shapeless.{::, HList, HNil, Poly1}
import shapeless.ops.hlist.IsHCons.Aux
import shapeless.ops.hlist.{IsHCons, Mapper}
import shapeless.ops.hlist.Mapper.Aux

/**
  * Created by Admin on 2017/4/26.
  */
//Double
object DoubleType extends DBType {
  override val typeName: String = "double"
  override val sqlType: Int = Types.DOUBLE
  override type PolyF = setDouble.type

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

  object setDouble extends Poly1 {
    implicit val doubleCase: Case.Aux[(PreparedStatement, Int, Double), PreparedStatement] =
      at({case (pstmt, index, v) => pstmt.setDouble(index, v); pstmt})
    implicit val numberCase: Case.Aux[(PreparedStatement, Int, Number), PreparedStatement] =
      at({case (pstmt, index, v) => pstmt.setDouble(index, v.doubleValue()); pstmt})
    implicit val stringCase: Case.Aux[(PreparedStatement, Int, String), PreparedStatement] =
      at({case (pstmt, index, v) => pstmt.setDouble(index, if(v.trim.isEmpty) 0D else v.toDouble); pstmt})
    implicit val DoubleCase: Case.Aux[(PreparedStatement, Int, java.lang.Double), PreparedStatement] =
      at({case (pstmt, index, v) => pstmt.setDouble(index, v.doubleValue()); pstmt})
  }
}
