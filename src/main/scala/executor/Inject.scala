package executor

import java.sql.ResultSet
import shapeless.{::, Generic, HList, HNil}
import shapeless.ops.hlist.FillWith

/**
  * Created by xiaziyao on 2017/4/18.
  */
trait Inject[L <: HList] {
  //    type Out = L
  def inject(l: L, rs: ResultSet, index: Int): L
}
object Inject {
  //    type Aux[T <: HList] = Inject[T] { type Out = T }
  def apply[R <: HList](implicit i: Inject[R]) = i

  //A :: HNil
  implicit def hcons0[A]
  (implicit c: shapeless.poly.Case[readField.type, (ResultSet, A, Int) :: HNil]): Inject[A :: HNil] =
    new Inject[A :: HNil] {
      override def inject(l : A :: HNil, rs: ResultSet, index: Int) = {
        readField.apply((rs, l.head, index)).asInstanceOf[A] :: HNil
      }
    }
  //A :: T
  implicit def hcons[T <: HList, A, Tail <: HList]
  (implicit c: shapeless.poly.Case[readField.type, (ResultSet, A, Int) :: HNil],
   tInject: Inject[T]): Inject[A :: T] =
    new Inject[A :: T] {
      override def inject(l : A :: T, rs: ResultSet, index: Int) = {
        readField.apply((rs, l.head, index)).asInstanceOf[A] :: tInject.inject(l.tail, rs, index + 1)
      }
    }

  def inject[T](rs: ResultSet)(implicit from: From[T]): T = from.to(rs)

}

trait From[T] {
  def to(rs: ResultSet): T
}
object From {
  implicit def from[T, R <: HList, O <: HList]
  (implicit generic: Generic.Aux[T, R],
   fill: FillWith[fillField.type, R] {type Out = O},
   inject: Inject[O],
   gen2: Generic.Aux[T, O]): From[T] = {
    val init = fill.apply()
    new From[T] {
      def to(rs: ResultSet): T = {
        val r = inject.inject(init, rs, 1)
        gen2.from(r)
      }
    }
  }
}

object readField extends shapeless.Poly1 {
  implicit def readInt: Case.Aux[(ResultSet, Int, Int), Int] = at(i => i._1.getInt(i._3))
  implicit def readShort: Case.Aux[(ResultSet, Short, Int), Short] = at(i => i._1.getShort(i._3))
  implicit def readLong: Case.Aux[(ResultSet, Long, Int), Long] = at(i => i._1.getLong(i._3))
  implicit def readFloat: Case.Aux[(ResultSet, Float, Int), Float] = at(i => i._1.getFloat(i._3))
  implicit def readDouble: Case.Aux[(ResultSet, Double, Int), Double] = at(i => i._1.getDouble(i._3))
  implicit def readBigDecimal: Case.Aux[(ResultSet, java.math.BigDecimal, Int), java.math.BigDecimal] = at(i => i._1.getBigDecimal(i._3))

  implicit def readBoolean: Case.Aux[(ResultSet, Boolean, Int), Boolean] = at(i => i._1.getBoolean(i._3))

  implicit def readString: Case.Aux[(ResultSet, String, Int), String] = at(i => i._1.getString(i._3))

  implicit def readSqlDate: Case.Aux[(ResultSet, java.sql.Date, Int), java.sql.Date] = at(i => i._1.getDate(i._3))
  implicit def readDate: Case.Aux[(ResultSet, java.util.Date, Int), java.util.Date] = at(i => i._1.getDate(i._3))
  implicit def readTimeStamp: Case.Aux[(ResultSet, java.sql.Timestamp, Int), java.sql.Timestamp] = at(i => i._1.getTimestamp(i._3))
  implicit def readTime: Case.Aux[(ResultSet, java.sql.Time, Int), java.sql.Time] = at(i => i._1.getTime(i._3))

  implicit def readArrayByte: Case.Aux[(ResultSet, Array[Byte], Int), Array[Byte]] = at({case (rs, _, index) =>
//    if (rs.getMetaData().getColumnType(index) == java.sql.Types.BLOB
//      || rs.getMetaData().getColumnType(index) == java.sql.Types.BINARY
//      || rs.getMetaData().getColumnType(index) == java.sql.Types.VARBINARY
//      || rs.getMetaData().getColumnType(index) == java.sql.Types.LONGVARBINARY
//      || rs.getMetaData().getColumnType(index) == java.sql.Types.ARRAY) {
//      val blob = rs.getBlob(index)
//      blob.getBytes(1, blob.length.toInt)
//    } else {
//      ???
//    }
    val blob = rs.getBlob(index)
    blob.getBytes(1, blob.length.toInt)
  })
  implicit def readBlob: Case.Aux[(ResultSet, java.sql.Blob, Int), java.sql.Blob] = at({case (rs, _, index) =>
      rs.getBlob(index)
  })
  implicit def readClob: Case.Aux[(ResultSet, java.sql.Clob, Int), java.sql.Clob] = at({case (rs, _, index) =>
    rs.getClob(index)
  })
  implicit def readOption[T](implicit rd: Case.Aux[(ResultSet, T, Int), T]) = at[(ResultSet, Option[T], Int)](i =>
//  {Option(readField((i._1, i._2.get, i._3)))}
    {Option(readField((i._1, i._2.getOrElse(null.asInstanceOf[T]), i._3)))}
  )

//  implicit def readSome[T](implicit rd: Case.Aux[(ResultSet, T, Int), T]) = at[(ResultSet, Some[T], Int)](i =>
//    //  {Option(readField((i._1, i._2.get, i._3)))}
//  {Option(readField((i._1, i._2.getOrElse(null.asInstanceOf[T]), i._3)))}
//  )
  //TODO add get other type
}
object fillField extends shapeless.Poly0 {
  implicit def caseByte = at(null.asInstanceOf[Byte]) //Byte
  implicit def caseInt = at[Int](0)
  implicit def caseShort = at[Short](null.asInstanceOf[Short]) //Short
  implicit def caseFloat = at[Float](null.asInstanceOf[Float]) //Float
  implicit def caseDouble = at[Double](null.asInstanceOf[Double]) //Double
  implicit def caseBigDecimal = at[java.math.BigDecimal](null.asInstanceOf[java.math.BigDecimal])
  implicit def caseLong = at[Long](null.asInstanceOf[Long]) //Long

  implicit def caseBoolean = at[Boolean](null.asInstanceOf[Boolean])

  implicit def caseString = at[String]("")
//  implicit def caseOption[T](implicit c: Case0[T]) = at[Option[T]](Some(fillField.apply[T]))


  implicit def caseDate = at[java.util.Date](null.asInstanceOf[java.util.Date])
  implicit def caseSQLTimestamp = at[java.sql.Timestamp](null.asInstanceOf[java.sql.Timestamp])
  implicit def caseSQLTime = at[java.sql.Time](null.asInstanceOf[java.sql.Time])

  implicit def caseSQLDate = at[java.sql.Date](null.asInstanceOf[java.sql.Date])

  //Array[Char] -> SqlChar ???

  implicit def caseArrayByte = at[Array[Byte]](null.asInstanceOf[Array[Byte]])
  implicit def caseBlob = at[java.sql.Blob](null.asInstanceOf[java.sql.Blob])

  implicit def caseClob = at[java.sql.Clob](null.asInstanceOf[java.sql.Clob])

  implicit def caseAnyRef = at[AnyRef](null.asInstanceOf[AnyRef])

  implicit def caseOption[T] = at[Option[T]](None)

//  implicit def caseSome[T](implicit c: Case0[T]) = at[Some[T]](Some(fillField.apply[T]))

}
