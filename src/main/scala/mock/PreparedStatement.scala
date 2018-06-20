package mock

import java.io.InputStream

class PreparedStatement1 { //Fixme just for test
  var i = new scala.collection.mutable.ListBuffer[Int]()
  var v = new scala.collection.mutable.ListBuffer[String]()
  def setInt(_i: Int, _v: Int): Unit = {
    i += _i
    v += _v.toString + ": Int"
  }
  def setLong(_i: Int, _v: Long): Unit = {
    i += _i
    v += _v.toString + ": Long"
  }
  def setDouble(_i: Int, _v: Double): Unit = {
    i += _i
    v += _v.toString + ": Double"
  }
  def setBinaryStream(parameterIndex: Int, x: InputStream, length: Int) = {
    i += parameterIndex
    v += "x : InputStream"
    println("DEBUG - setBinaryStream")
  }
  def setBytes(_i: Int, _v: Array[Byte]): Unit = {
    i += _i
    v += "[...] : Array[Byte]"
  }
  def setBlob(parameterIndex: Int, inputStream: InputStream): Unit = {
    i += parameterIndex
    v += "x : InputStream"
    println("DEBUG - setBlob")
  }
  def setBlob(parameterIndex: Int, blob: java.sql.Blob): Unit = {
    i += parameterIndex
    v += "x : Blob"
    println("DEBUG - setBlob")
  }
  def setFloat(_i: Int, _v: Float): Unit = {
    i += _i
    v += _v.toString + ": Float"
  }
  def setBoolean(_i: Int, _v: Boolean): Unit = {
    i += _i
    v += _v.toString + ": Boolean"
  }
  def setString(_i: Int, _v: String): Unit = {
    i += _i
    v += _v + ": String"
  }
  def setNull(_i: Int, _t: Int): Unit = {
    i += _i
    v += "-1" + s": Null sqlType=${_t}"
  }

  def setObject(_i: Int, _t: Object): Unit = {
    i += _i
    v += _t + ": Object"
  }

  def setDate(_i: Int, _t: java.sql.Date): Unit = {
    i += _i
    v += _t + ": java.sql.Date"
  }

  def executeUpdate() = 100

  def getGeneratedKeys(): ResultSet = new ResultSet(100)

  def addBatch() = {println("DEBUG - addBatch")}

  def executeBatch() : Array[Int] = Array()

  def close() = {println("DEBUG - PreparedStatement close")}
  override def toString: String = s"PreparedStatement(index = [${i.mkString(",")}], value = [${v.mkString(",")}])"
}
