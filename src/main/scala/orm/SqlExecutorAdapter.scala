package orm

import ds.DataSource
import executor.{MySqlExecutor, SqlExecutor}
import pool.{ConnectionPool, DruidPool}

/**
  * Created by Admin on 2017/4/25.
  */
trait SqlExecutorAdapter {
  val dbRef: String
  val pool: ConnectionPool
  val sqlExecutor: SqlExecutor
  val dataSource: DataSource
}

object DefaultSqlAdapter extends SqlExecutorAdapter {
  override val dbRef: String = "druid"
  override val pool: ConnectionPool = new DruidPool(dbRef)
  override val sqlExecutor: SqlExecutor = new MySqlExecutor(dbRef)
  override val dataSource: DataSource = new DataSource(dbRef)
}
