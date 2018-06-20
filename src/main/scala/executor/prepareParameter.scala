package executor

import java.sql.PreparedStatement

import dbtype._
import parameter.Parameter
import shapeless.{Poly1, Poly2}

/**
  * Created by xiaziyao on 2017/5/23.
  */
//eq getFieldType
object prepareParameter extends Poly2 {
  implicit def caseByte: Case.Aux[PreparedStatement, Parameter[Byte, ByteType.PolyF], PreparedStatement] =
    at((pstmt, param) => param.prepare2(pstmt))

  implicit def caseInt: Case.Aux[PreparedStatement, Parameter[Int, IntType.PolyF], PreparedStatement] =
    at((pstmt, param) => param.prepare2(pstmt))
  implicit def caseInteger: Case.Aux[PreparedStatement, Parameter[Integer, IntType.PolyF], PreparedStatement] =
    at((pstmt, param) => param.prepare2(pstmt))

  implicit def caseShort: Case.Aux[PreparedStatement, Parameter[Short, SmallIntType.PolyF], PreparedStatement] =
    at((pstmt, param) => param.prepare2(pstmt))

  implicit def caseFloat: Case.Aux[PreparedStatement, Parameter[Float, FloatType.PolyF], PreparedStatement] =
    at((pstmt, param) => param.prepare2(pstmt))

  implicit def caseDouble: Case.Aux[PreparedStatement, Parameter[Double, DoubleType.PolyF], PreparedStatement] =
    at((pstmt, param) => param.prepare2(pstmt))

  implicit def caseBigDecimal: Case.Aux[PreparedStatement, Parameter[java.math.BigDecimal, DecimalType.PolyF], PreparedStatement] =
    at((pstmt, param) => param.prepare2(pstmt))

  implicit def caseLong: Case.Aux[PreparedStatement, Parameter[Long, BigIntType.PolyF], PreparedStatement] =
    at((pstmt, param) => param.prepare2(pstmt))

  implicit def caseBoolean: Case.Aux[PreparedStatement, Parameter[Boolean, BooleanType.PolyF], PreparedStatement] =
    at((pstmt, param) => param.prepare2(pstmt))

  implicit def caseString: Case.Aux[PreparedStatement, Parameter[String, Varchar.PolyF], PreparedStatement] =
    at((pstmt, param) => param.prepare2(pstmt))

  implicit def caseDate: Case.Aux[PreparedStatement, Parameter[java.util.Date, TimeStampType.PolyF], PreparedStatement] =
    at((pstmt, param) => param.prepare2(pstmt))
  implicit def caseSQLTimestamp: Case.Aux[PreparedStatement, Parameter[java.sql.Timestamp, TimeStampType.PolyF], PreparedStatement] =
    at((pstmt, param) => param.prepare2(pstmt))
  implicit def caseSQLTime: Case.Aux[PreparedStatement, Parameter[java.sql.Time, TimeStampType.PolyF], PreparedStatement] =
    at((pstmt, param) => param.prepare2(pstmt))
  implicit def caseSQLDate: Case.Aux[PreparedStatement, Parameter[java.sql.Date, DateType.PolyF], PreparedStatement] =
    at((pstmt, param) => param.prepare2(pstmt))

  implicit def caseArrayByte: Case.Aux[PreparedStatement, Parameter[Array[Byte], BlobType.PolyF], PreparedStatement] =
    at((pstmt, param) => param.prepare2(pstmt))

  implicit def caseBlob: Case.Aux[PreparedStatement, Parameter[java.sql.Blob, BlobType.PolyF], PreparedStatement] =
    at((pstmt, param) => param.prepare2(pstmt))

  implicit def caseClob: Case.Aux[PreparedStatement, Parameter[java.sql.Clob, ClobType.PolyF], PreparedStatement] =
    at((pstmt, param) => param.prepare2(pstmt))


  implicit def caseAnyRef: Case.Aux[PreparedStatement, Parameter[AnyRef, UnknownType.PolyF], PreparedStatement] =
    at((pstmt, param) => param.prepare2(pstmt))


  //Option
  implicit def caseOption[T, PolyF <: Poly1](implicit c: Case.Aux[PreparedStatement, Parameter[T, PolyF], PreparedStatement]) =
    at[PreparedStatement, Parameter[Option[T], PolyF]]((pstmt, param) =>
      prepareParameter.apply(pstmt, new Parameter[T, PolyF](param.name, param.value.getOrElse(null.asInstanceOf[T]), param.indexes, param.dbType)))

//  implicit def caseSome[T, PolyF <: Poly1](implicit c: Case.Aux[PreparedStatement, Parameter[T, PolyF], PreparedStatement]) =
//    at[PreparedStatement, Parameter[Some[T], PolyF]]((pstmt, param) =>
//      prepareParameter.apply(pstmt, new Parameter[T, PolyF](param.name, param.value.get, param.indexes, param.dbType)))

  implicit def default[T, P <: Poly1]: Case.Aux[PreparedStatement, Parameter[T, P], PreparedStatement] =
    at((_, param) => throw new IllegalArgumentException(s"can't prepare $param"))
}

