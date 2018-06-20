package dbtype
import java.sql.{PreparedStatement, Time, Timestamp, Types}

import shapeless.{::, HList, HNil, Poly1}
import shapeless.ops.hlist.IsHCons.Aux
import shapeless.ops.hlist.{IsHCons, Mapper}
import shapeless.ops.hlist.Mapper.Aux

/**
  * Created by Admin on 2017/4/26.
  */
//java.util.Date | sql.TimeStamp | sql.Time
object TimeStampType extends DBType {
  override val typeName: String = "timestamp"
  override val sqlType: Int = Types.TIMESTAMP
  override type PolyF = setTimestamp.type

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

  object setTimestamp extends Poly1 {
    implicit val timestampCase: Case.Aux[(PreparedStatement, Int, Timestamp), PreparedStatement] =
      at({case (pstmt, index, v) => pstmt.setTimestamp(index, v); pstmt})
    implicit val dateCase: Case.Aux[(PreparedStatement, Int, java.util.Date), PreparedStatement] =
      at({case (pstmt, index, v) =>
        val timestamp = new Timestamp(v.getTime)
        pstmt.setTimestamp(index, timestamp)
        pstmt
      })
    implicit val stringCase: Case.Aux[(PreparedStatement, Int, String), PreparedStatement] =
      at({case (pstmt, index, v) =>
        //TODO
        ???
      })
    implicit val TimeCase: Case.Aux[(PreparedStatement, Int, Time), PreparedStatement] =
      at({case (pstmt, index, v) => pstmt.setTime(index, v); pstmt})
  }
}
