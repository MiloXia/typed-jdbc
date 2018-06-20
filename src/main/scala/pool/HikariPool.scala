package pool

import javax.sql.DataSource

/**
  * Created by xiaziyao on 2017/4/12.
  */
class HikariPool(poolName: String) extends ConnectionPool(poolName) {
  override val dataSource: DataSource = ???
}
