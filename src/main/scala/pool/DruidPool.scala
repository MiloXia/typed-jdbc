package pool

import java.sql.SQLException
import javax.sql.DataSource

import com.alibaba.druid.pool.DruidDataSource

/**
  * Created by xiaziyao on 2017/4/12.
  */
class DruidPool(poolName: String) extends ConnectionPool(poolName) {
  import DruidPool._
  override lazy val dataSource: DataSource = {
    val druidDataSource = new DruidDataSource()
    druidDataSource.setUsername(config.getString("username"))
    druidDataSource.setUrl(config.getString("jdbcUrl"))
    druidDataSource.setPassword(config.getString("password"))
    druidDataSource.setDriverClassName(config.getString("driverClass"))
    druidDataSource.setInitialSize(config.getInt("initialSize"))
    druidDataSource.setMaxActive(config.getInt("maxActive"))
    druidDataSource.setMaxWait(config.getInt("maxWait"))
    druidDataSource.setMinIdle(config.getInt("minIdle"))

    try {
      druidDataSource.setFilters(config.getString("filters"))
    } catch {
      case e: SQLException => e.printStackTrace();
    }
    druidDataSource.setTestWhileIdle(config.getBoolean("testWhileIdle"))
    druidDataSource.setTestOnReturn(config.getBoolean("testOnReturn"))
    druidDataSource.setTestOnBorrow(config.getBoolean("testOnBorrow"))
    druidDataSource
  }
}

object DruidPool {
  val config = ConfigFactory.config.getConfig("db")

//  def main(args: Array[String]): Unit = {
//    println(ConfigFactory.config.getString("db.username"))
//  }
}
