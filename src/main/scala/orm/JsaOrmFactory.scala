package orm

/**
  * Created by Admin on 2017/4/25.
  */
object DBFactory {
  lazy val sqlExecutorMap: Map[String, SqlExecutorAdapter] = Map(
    DefaultSqlAdapter.dbRef -> DefaultSqlAdapter
  )

  def getSqlExecutorAdapter(dbRef: String): SqlExecutorAdapter = sqlExecutorMap.getOrElse(dbRef, DefaultSqlAdapter)
  //TODO other method
}
