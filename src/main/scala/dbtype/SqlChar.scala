package dbtype
import java.sql.{PreparedStatement, Types}

import shapeless.{::, HList, HNil, Poly1}
import shapeless.ops.hlist.IsHCons.Aux
import shapeless.ops.hlist.{IsHCons, Mapper}
import shapeless.ops.hlist.Mapper.Aux

/**
  * Created by Admin on 2017/4/26.
  */
//String + Length fixed = true
object SqlChar extends DBType {
  override val typeName: String = "char"
  override val sqlType: Int = Types.CHAR
  override type PolyF = setString.type

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

  object setString extends Poly1 {
    //    implicit val intCase: Case.Aux[(PreparedStatement1, Int, Int), PreparedStatement1] =
    //      at({case (pstmt, index, v) => pstmt.setInt(index, v); pstmt})
    //    implicit val numberCase: Case.Aux[(PreparedStatement1, Int, Number), PreparedStatement1] =
    //      at({case (pstmt, index, v) => pstmt.setInt(index, v.intValue()); pstmt})
    implicit val stringCase: Case.Aux[(PreparedStatement, Int, String), PreparedStatement] =
    at({case (pstmt, index, v) => pstmt.setString(index, v); pstmt})
    //    implicit val integerCase: Case.Aux[(PreparedStatement1, Int, Integer), PreparedStatement1] =
    //      at({case (pstmt, index, v) => pstmt.setInt(index, v.intValue()); pstmt})
  }
}
