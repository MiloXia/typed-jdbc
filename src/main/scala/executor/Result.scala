package executor

import java.sql.{Connection, ResultSet, PreparedStatement}

import scala.collection.mutable.ListBuffer

/**
  * Created by xiaziyao on 2017/4/21.
  */
class Result[T](private val resultSet: ResultSet,
                private val conn: Connection,
                private val pstmt: PreparedStatement) {
  def as[T2](implicit from: From[T2]): Option[T2] = {
    try {
      val res = if(resultSet.next()) Some(from.to(resultSet)) else None
      pstmt.close()
      resultSet.close() //immediately
      res
    } catch {
      case e: Exception => throw new RuntimeException(e)
    } finally {
       conn.close()
    }
  }
  def result(implicit from: From[T]): Option[T] = {
    try {
      val res = if(resultSet.next()) Some(from.to(resultSet)) else None
      pstmt.close()
      resultSet.close() //immediately
      res
    } catch {
      case e: Exception => throw new RuntimeException(e)
    } finally {
      conn.close()
    }
  }
}

class ResultSeq[T](private val resultSet: ResultSet,
                   private val conn: Connection,
                   private val pstmt: PreparedStatement) {

  def as[T2](implicit from: From[T2]): Seq[T2] = {
    try {
      val seq1 = new ListBuffer[T2]()
      while (resultSet.next()) {
        val x = from.to(resultSet)
        seq1 += x
      }
      pstmt.close()
      resultSet.close() //immediately
      seq1.toSeq
    } catch {
      case e: Exception => throw new RuntimeException(e)
    } finally {
      conn.close()
    }
  }
  def result(implicit from: From[T]): Seq[T] = {
    try {
      val seq1 = new ListBuffer[T]()
      while (resultSet.next()) {
        val x = from.to(resultSet)
        seq1 += x
      }
      pstmt.close() //TODO cache pstmt
      resultSet.close() //immediately
      seq1.toList
    } catch {
      case e: Exception => throw new RuntimeException(e)
    } finally {
      conn.close()
    }
  }
}
