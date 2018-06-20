package dbtype

import java.sql.Types

import java.sql.PreparedStatement
import shapeless.Poly1
import shapeless.ops.hlist.IsHCons

/**
  * Created by xiaziyao on 2017/4/13.
  */
object IntType extends DBType {
  val typeName: String = "int"
  val sqlType = Types.INTEGER
  override def setParameter[T](pstmt: PreparedStatement, index: Int, value: T): PreparedStatement = ???

  override type PolyF = setTnt.type

  import shapeless._
  import shapeless.ops.hlist.Mapper
  override def setParameter2[T, O <: HList, PolyF2 <: Poly1](pstmt: PreparedStatement, index: Int, value: T)
                                           (implicit mapper: Mapper.Aux[PolyF2, (PreparedStatement, Int, T) :: HNil, O],
                                            isCons: IsHCons.Aux[O, PreparedStatement, HNil]): PreparedStatement = {
    if (value == null) {
      pstmt.setNull(index, sqlType)
      pstmt
    } else {
//      println("set Int")
      mapper((pstmt, index, value) :: HNil)
      pstmt
    }
  }

  object setTnt extends Poly1 {
    implicit val intCase: Case.Aux[(PreparedStatement, Int, Int), PreparedStatement] =
      at({case (pstmt, index, v) => pstmt.setInt(index, v); pstmt})
    implicit val numberCase: Case.Aux[(PreparedStatement, Int, Number), PreparedStatement] =
      at({case (pstmt, index, v) => pstmt.setInt(index, v.intValue()); pstmt})
    implicit val stringCase: Case.Aux[(PreparedStatement, Int, String), PreparedStatement] =
      at({case (pstmt, index, v) => pstmt.setInt(index, if(v.trim.isEmpty) 0 else v.toInt); pstmt})
    implicit val integerCase: Case.Aux[(PreparedStatement, Int, Integer), PreparedStatement] =
      at({case (pstmt, index, v) => pstmt.setInt(index, v.intValue()); pstmt})
  }
}
