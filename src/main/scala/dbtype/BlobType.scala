package dbtype
import java.io._
import java.sql.{SQLException, Types}

import java.sql.PreparedStatement
import shapeless.{::, HList, HNil, Poly1}
import shapeless.ops.hlist.IsHCons.Aux
import shapeless.ops.hlist.{IsHCons, Mapper}
import shapeless.ops.hlist.Mapper.Aux

import java.io.File

/**
  * Created by Admin on 2017/4/26.
  */
//Array[Byte] | File | Blob
object BlobType extends DBType {
  override val typeName: String = "blob"
  override val sqlType: Int = Types.BLOB
  override type PolyF = setBinaryStream.type

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

  object setBinaryStream extends Poly1 {
    private def toBinary(index: Int, v: Object, pstmt: PreparedStatement) = {
      val bos = new ByteArrayOutputStream
      try {
        val OOs = new ObjectOutputStream(bos)
        OOs.writeObject(v)
        OOs.close()

        val in: InputStream = new ByteArrayInputStream(bos.toByteArray)
        pstmt.setBinaryStream(index, in, bos.size())
      } catch {
        case e: Exception =>
          // TODO Auto-generated catch block
          throw new SQLException(e)
      }
    }
    implicit val arrayByteCase: Case.Aux[(PreparedStatement, Int, Array[Byte]), PreparedStatement] =
      at({case (pstmt, index, v) =>
//        toBinary(index, v, pstmt)
        pstmt.setBytes(index, v)
        pstmt
      })
    implicit val javaArrayByteCase: Case.Aux[(PreparedStatement, Int, Array[java.lang.Byte]), PreparedStatement] =
      at({case (pstmt, index, v) =>
        toBinary(index, v, pstmt)
        pstmt
      })

    implicit val fileCase: Case.Aux[(PreparedStatement, Int, File), PreparedStatement] =
      at({case (pstmt, index, v) =>
        val in = new BufferedInputStream(new FileInputStream(v))
        pstmt.setBlob(index, in)
        pstmt
      })
    implicit val blobCase: Case.Aux[(PreparedStatement, Int, java.sql.Blob), PreparedStatement] =
      at({case (pstmt, index, v) =>
        pstmt.setBlob(index, v)
        pstmt
      })
  }

}
