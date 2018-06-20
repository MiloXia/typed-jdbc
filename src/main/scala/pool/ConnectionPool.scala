package pool

import javax.sql.DataSource

/**
  * Created by xiaziyao on 2017/4/12.
  */
abstract class ConnectionPool(val poolName: String) {
  val dataSource: DataSource
}

object ConnectionPool {

}
