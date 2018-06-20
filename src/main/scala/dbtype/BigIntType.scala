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
//Long
object BigIntType extends DBType {
  override val typeName: String = "bigint"
  override val sqlType: Int = Types.BIGINT
  override type PolyF = setLong.type

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

  object setLong extends Poly1 {
    implicit val longCase: Case.Aux[(PreparedStatement, Int, Long), PreparedStatement] =
      at({case (pstmt, index, v) => pstmt.setLong(index, v); pstmt})
    implicit val numberCase: Case.Aux[(PreparedStatement, Int, Number), PreparedStatement] =
      at({case (pstmt, index, v) => pstmt.setLong(index, v.longValue()); pstmt})
    implicit val stringCase: Case.Aux[(PreparedStatement, Int, String), PreparedStatement] =
      at({case (pstmt, index, v) => pstmt.setLong(index, if(v.trim.isEmpty) 0L else v.toLong); pstmt})
    implicit val LongCase: Case.Aux[(PreparedStatement, Int, java.lang.Long), PreparedStatement] =
      at({case (pstmt, index, v) => pstmt.setLong(index, v.longValue()); pstmt})
  }
}
