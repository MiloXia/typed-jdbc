package mock

import java.sql.Types

/**
  * Created by xiaziyao on 2017/4/17.
  */
class ResultSet(size: Int) {
  var rest = size
  def next() : Boolean = {
    val r = rest > 0
    if(r) rest -= 1
    r
  }
  def getInt(i: Int) = i
  def getLong(i: Int) = i.toLong
  def getFloat(i: Int) = i.toFloat
  def getDouble(i: Int) = i.toDouble
  def getDate(i: Int): java.sql.Date = new java.sql.Date(System.currentTimeMillis())
  def getString(i: Int) = "str_" + i.toString

  def close() = {println("DEBUG - rs close")}
  //...

  def getBlob(columnIndex: Int) = new javax.sql.rowset.serial.SerialBlob("".getBytes())

  def getMetaData(): ResultSetMetaData = new ResultSetMetaData
}

class Blob {
  def getBytes(pos: Long, length: Int): Array[Byte] = Array()
  def length = 1
}

class ResultSetMetaData {
  def getColumnType(column: Int) = Types.BLOB
}