package dbtype
import java.sql.{PreparedStatement, Types}

import shapeless.{::, HList, HNil, Poly1}
import shapeless.ops.hlist.IsHCons.Aux
import shapeless.ops.hlist.{IsHCons, Mapper}
import shapeless.ops.hlist.Mapper.Aux

/**
  * Created by Admin on 2017/4/26.
  */
//Short
object SmallIntType extends DBType {
  override val typeName: String = "smallint"
  override val sqlType: Int = Types.SMALLINT
  override type PolyF = setShort.type

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

  object setShort extends Poly1 {
    implicit val shortCase: Case.Aux[(PreparedStatement, Int, Short), PreparedStatement] =
      at({case (pstmt, index, v) => pstmt.setShort(index, v); pstmt})
    implicit val numberCase: Case.Aux[(PreparedStatement, Int, Number), PreparedStatement] =
      at({case (pstmt, index, v) => pstmt.setShort(index, v.shortValue()); pstmt})
    implicit val stringCase: Case.Aux[(PreparedStatement, Int, String), PreparedStatement] =
      at({case (pstmt, index, v) => pstmt.setShort(index, if(v.trim.isEmpty) 0.toShort else v.toShort); pstmt})
    implicit val ShortCase: Case.Aux[(PreparedStatement, Int, java.lang.Short), PreparedStatement] =
      at({case (pstmt, index, v) => pstmt.setShort(index, v.shortValue()); pstmt})
  }
}
