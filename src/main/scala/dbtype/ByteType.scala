package dbtype

import java.sql.{SQLException, Types}

import java.sql.PreparedStatement
import shapeless._
import shapeless.ops.hlist.Mapper
import shapeless.ops.hlist.IsHCons

/**
  * Created by xiaziyao on 2017/4/12.
  */
object ByteType extends DBType {
  val typeName: String = "tinyint"
  val sqlType: Int = Types.TINYINT

  override def setParameter[T](pstmt: PreparedStatement, index: Int, value: T): PreparedStatement = {
    if(value == null) {
      pstmt.setNull(index, sqlType)
      pstmt
    } else {
      value match {
        case v: Int => //TODO isInstanceOf is not good
          pstmt.setInt(index, v)
        case v: Integer =>
          pstmt.setInt(index, v.intValue())
        case v: String =>
          pstmt.setInt(index, if(v.trim.isEmpty) 0 else v.toInt)
        case _ => throw new SQLException("data format error")
      }
      pstmt
    }
  }

  type PolyF = setTnt2.type

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

  //constraint of value type, use compile error instead of runtime SQLException
  object setTnt2 extends Poly1 {
    implicit val intCase: Case.Aux[(PreparedStatement, Int, Int), PreparedStatement] =
      at({case (pstmt, index, v) => pstmt.setInt(index, v); pstmt})
    implicit val stringCase: Case.Aux[(PreparedStatement, Int, String), PreparedStatement] =
      at({case (pstmt, index, v) => pstmt.setInt(index, if(v.trim.isEmpty) 0 else v.toInt); pstmt})
    implicit val integerCase: Case.Aux[(PreparedStatement, Int, Integer), PreparedStatement] =
      at({case (pstmt, index, v) => pstmt.setInt(index, v.intValue()); pstmt})
    implicit val javaByteCase: Case.Aux[(PreparedStatement, Int, java.lang.Byte), PreparedStatement] =
      at({case (pstmt, index, v) => pstmt.setInt(index, v.intValue()); pstmt})
    implicit val byteCase: Case.Aux[(PreparedStatement, Int, Byte), PreparedStatement] =
      at({case (pstmt, index, v) => pstmt.setInt(index, v); pstmt})
  }



//  def main(args: Array[String]): Unit = {
//    val p = ByteType.setParameter3(new PreparedStatement, 1, "2")
//    println(p)
//    val p2 = UnknownType.setParameter3(new PreparedStatement, 1, "2".asInstanceOf[AnyRef])
//    println(p2)
//
//    println(Character.isJavaIdentifierPart(' '))
//    println("abc".substring(1, 2))
//  }

}
