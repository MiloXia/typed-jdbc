package dbtype
import java.sql.{Date, Types}

import java.sql.PreparedStatement
import shapeless.{::, HList, HNil, Poly1}
import shapeless.ops.hlist.{IsHCons, Mapper}

/**
  * Created by Admin on 2017/4/26.
  */
//java.sql.Date
object DateType extends DBType {
  override val typeName: String = "date"
  override val sqlType: Int = Types.DATE
  override type PolyF = setDate.type

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

  object setDate extends Poly1 {
    implicit val sqlDateCase: Case.Aux[(PreparedStatement, Int, java.sql.Date), PreparedStatement] =
      at({case (pstmt, index, v) => pstmt.setDate(index, v); pstmt})
    implicit val stringCase: Case.Aux[(PreparedStatement, Int, String), PreparedStatement] =
      at({case (pstmt, index, v) =>
        pstmt.setBoolean(index, if(v.trim == "1") true else if(v.trim == "0" || v.trim.isEmpty) false else v.toBoolean)
        if(v.trim == "") {
          pstmt.setNull(index, sqlType)
        } else {
          val date = java.sql.Date.valueOf(v)
          pstmt.setDate(index, new Date(date.getTime))
        }
        pstmt
      })
    implicit val javaDateCase: Case.Aux[(PreparedStatement, Int, java.util.Date), PreparedStatement] =
      at({case (pstmt, index, v) => pstmt.setDate(index, new java.sql.Date(v.getTime)); pstmt})
  }
}
