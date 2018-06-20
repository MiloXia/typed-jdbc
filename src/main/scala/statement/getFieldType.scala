package statement

import dbtype._

/**
  * mapping of Scala/Java to SQL type.
  */
//old DBTypeFactory.getDbType, Class -> DBType
object getFieldType extends shapeless.Poly1 {
  implicit def caseByte: Case.Aux[Byte, ByteType.type] = at(_ => ByteType) //Byte
  implicit def caseInt: Case.Aux[Int, IntType.type ] = at(_ => IntType) //Int
  implicit def caseShort: Case.Aux[Short, SmallIntType.type] = at(_ => SmallIntType) //Short
  implicit def caseFloat: Case.Aux[Float, FloatType.type] = at(_ => FloatType) //Float
  implicit def caseDouble: Case.Aux[Double, DoubleType.type] = at(_ => DoubleType) //Double
  implicit def caseBigDecimal: Case.Aux[java.math.BigDecimal, DecimalType.type] = at(_ => DecimalType)
  implicit def caseLong: Case.Aux[Long, BigIntType.type] = at(_ => BigIntType) //Long

  implicit def caseBoolean: Case.Aux[Boolean, BooleanType.type] = at(_ => BooleanType)

  implicit def caseString: Case.Aux[String, Varchar.type] = at(_ => Varchar) //String

  implicit def caseDate: Case.Aux[java.util.Date, TimeStampType.type] = at(_ => TimeStampType)
  implicit def caseSQLTimestamp: Case.Aux[java.sql.Timestamp, TimeStampType.type] = at(_ => TimeStampType)
  implicit def caseSQLTime: Case.Aux[java.sql.Time, TimeStampType.type] = at(_ => TimeStampType)

  implicit def caseSQLDate: Case.Aux[java.sql.Date, DateType.type] = at(_ => DateType)

  //Array[Char] -> SqlChar ???

  implicit def caseArrayByte: Case.Aux[Array[Byte], BlobType.type] = at(_ => BlobType)
  implicit def caseBlob: Case.Aux[java.sql.Blob, BlobType.type] = at(_ => BlobType)

  implicit def caseClob: Case.Aux[java.sql.Clob, ClobType.type] = at(_ => ClobType)

  implicit def caseAnyRef: Case.Aux[AnyRef, UnknownType.type] = at(_ => UnknownType)
//implicit def caseOptInt: Case.Aux[Option[Int], IntType.type] = at(_ => IntType) //Int
  //Option[T]
  implicit def caseOption[T](implicit c: Case[T]) = at[Option[T]](opt => getFieldType.apply[T](opt.getOrElse(null.asInstanceOf[T]))(c))

  implicit def caseSome[T](implicit c: Case[T]) = at[Some[T]](opt => getFieldType.apply[T](opt.get)(c))

  //TODO define other types mapper

}
