package dbtype
import java.sql.{PreparedStatement, Types}

import shapeless.{::, HList, HNil, Poly1}
import shapeless.ops.hlist.{IsHCons, Mapper}

/**
  * Created by Admin on 2017/4/26.
  */
object ClobType extends DBType {
  override val typeName: String = "clob"
  override val sqlType: Int = Types.CLOB
  override type PolyF = setClob.type

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

  object setClob extends Poly1 {
    implicit val arrayByteCase: Case.Aux[(PreparedStatement, Int, Array[Byte]), PreparedStatement] =
      at({case (pstmt, index, v) =>
        //        toBinary(index, v, pstmt)
        pstmt.setBytes(index, v)
        pstmt
      })
    implicit val javaArrayByteCase: Case.Aux[(PreparedStatement, Int, Array[java.lang.Byte]), PreparedStatement] =
      at({case (pstmt, index, v) =>
          //TODO
        ???
      })

    implicit val clobCase: Case.Aux[(PreparedStatement, Int, java.sql.Clob), PreparedStatement] =
      at({case (pstmt, index, v) =>
        pstmt.setClob(index, v)
        pstmt
      })
  }

}
