import java.sql.Connection
import java.sql.DriverManager
import java.sql.PreparedStatement
import java.sql.SQLException

object DBHelper {
  val url = "jdbc:mysql://127.0.0.1/student"
  val name = "com.mysql.jdbc.Driver"
  val user = "root"
  val password = "root"
}

class DBHelper(sql: String) {
  import DBHelper._
  var conn: Connection = _
  var pst: PreparedStatement = _

  try {
    Class.forName(name)//指定连接类型
    conn = DriverManager.getConnection(url, user, password)//获取连接
    pst = conn.prepareStatement(sql)//准备执行语句
  } catch {
    case e: Exception => e.printStackTrace()
  }

  def close() {
    try {
      this.conn.close()
      this.pst.close()
    } catch {
      case e: SQLException => e.printStackTrace()
    }
  }
}

object DBTest {
  def main(args: Array[String]) {
    val sql = "select * from user" //SQL语句
    val db1 = new DBHelper(sql) //创建DBHelper对象

    try {
      val ret: java.sql.ResultSet = db1.pst.executeQuery()//执行语句，得到结果集
      while (ret.next()) {
        val id = ret.getInt(1)
        val name = ret.getString(2)
        val age = ret.getInt(3)
        println(id + "\t" + name + "\t" + age)
      }//显示数据
      ret.close()
      db1.close()//关闭连接
    } catch {
      case e: SQLException => e.printStackTrace()
    }
  }
}
