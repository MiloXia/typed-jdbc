package executor
import java.sql.Connection
import javax.sql.DataSource

import dbtype._
import ds.ManipulationResult
import executor.SqlExecutor.Folder
import java.sql.{PreparedStatement, ResultSet}
import orm.JsaOrmFactory
import parameter.Parameter
import shapeless.labelled.FieldType
import shapeless.ops.hlist.{FillWith, IsHCons, LeftFolder, Mapper, Zip}
import shapeless.{::, Generic, HList, HNil, LabelledGeneric, Poly1, Poly2, Witness}
import vo.User
import statement.{Parameters, Statement, StatementType, getFieldType}

import scala.collection.mutable.ListBuffer

/**
  * Created by xiaziyao on 2017/4/14.
  */
class MySqlExecutor(val poolName: String) extends SqlExecutor {

//  def getOneByValue[T, R <: HList, O <: HList](statement: Statement[T])
//                                                       (implicit gen: LabelledGeneric.Aux[T, R],
//                                                        ps: Parameters.Aux[R, O],
//                                                        folder: LeftFolder[O, PreparedStatement, prepareParameter.type],
//                                                        from: From[T]
//                                                       ): Option[T] = {
//    val pstmt = new PreparedStatement
//    val params = statement.mkParameters(gen, ps)
//    println("DEBUG - sql : " + statement.parsedSql)
//    //TODO Filter is need ???
//    val pdstmt = folder(params, pstmt).asInstanceOf[PreparedStatement]
//    //getPreparedStatement(connection, statement.parsedSql.toString, statement.statType, statement.autoId)
//    //TODO exe with pdstmt
//    println("DEBUG - pdstmt : " + pdstmt)
//    val rs = new ResultSet(1)
//    if(rs.next()) Some(from.to(rs)) else None
//  }

  def getOneByValue[T, R <: HList, O <: HList](statement: Statement[T])
                                              (implicit gen: LabelledGeneric.Aux[T, R],
                                               ps: Parameters.Aux[R, O],
                                               folder: LeftFolder[O, PreparedStatement, prepareParameter.type]
                                              ): Result[T] = {
    val conn = getConnection
    try {
//      val pstmt = new PreparedStatement
      val pstmt = getPreparedStatement(conn, statement.parsedSql.toString, statement.statType, statement.autoId)
      val params = statement.mkParameters(gen, ps)
      println("DEBUG - sql : " + statement.parsedSql)
      //TODO Filter is need ???
      val pdstmt = folder(params, pstmt).asInstanceOf[PreparedStatement]
      //getPreparedStatement(connection, statement.parsedSql.toString, statement.statType, statement.autoId)
      //TODO exe with pdstmt
      println("DEBUG - pdstmt : " + pdstmt)
      val rs = pstmt.executeQuery()
//      val rs = new ResultSet(1)
//      pdstmt.close()
      new Result[T](rs, conn, pdstmt)
    } catch {
      case e: Exception => throw new RuntimeException(e)
    } finally {
//      conn.close()
    }

  }

  def getOneByValue2[T, R <: HList, O <: HList](statement: Statement[T])
  (implicit gen: Generic.Aux[T, R],
  ps: Parameters.Aux[R, O],
  folder: LeftFolder[O, PreparedStatement, prepareParameter.type]
  ): Result[T] = {
    val conn = getConnection
    try {
//      val pstmt = new PreparedStatement
      val pstmt = getPreparedStatement(conn, statement.parsedSql.toString, statement.statType, statement.autoId)
      val params = statement.mkParameters2(gen, ps)
      println("DEBUG - sql : " + statement.parsedSql)
      //TODO Filter is need ???
      val pdstmt = folder(params, pstmt).asInstanceOf[PreparedStatement]
      //getPreparedStatement(connection, statement.parsedSql.toString, statement.statType, statement.autoId)
      println("DEBUG - pdstmt : " + pdstmt)
//      val rs = new ResultSet(1)
      val rs = pdstmt.executeQuery()
//      pdstmt.close()
      new Result[T](rs, conn, pdstmt)
    } catch {
      case e: Exception => throw new RuntimeException(e)
    } finally {
//      conn.close()
    }

  }

  def getOneByValue3[K, V, O <: HList](statement: Statement[FieldType[K, V]])
                                               (implicit
                                                ps: Parameters.Aux[(FieldType[K, V] :: HNil), O],
                                                folder: LeftFolder[O, PreparedStatement, prepareParameter.type]
                                               ): Result[V] = {
    val conn = getConnection
    try {
//      val pstmt = new PreparedStatement
      val pstmt = getPreparedStatement(conn, statement.parsedSql.toString, statement.statType, statement.autoId)
      val params = statement.mkParameters3(ps)
      println("DEBUG - sql : " + statement.parsedSql)
      //TODO Filter is need ???
      val pdstmt = folder(params, pstmt).asInstanceOf[PreparedStatement]
      //getPreparedStatement(connection, statement.parsedSql.toString, statement.statType, statement.autoId)
      //TODO exe with pdstmt
      println("DEBUG - pdstmt : " + pdstmt)
//      val rs = new ResultSet(1)
      val rs = pstmt.executeQuery()
//      pdstmt.close()
      new Result[V](rs, conn, pdstmt)
    } catch {
      case e: Exception => throw new RuntimeException(e)
    } finally {
      //conn.close()
    }
  }

//  override def getByValue[T, R <: HList, O <: HList](statement: Statement[T])
//                                              (implicit gen: LabelledGeneric.Aux[T, R],
//                                               ps: Parameters.Aux[R, O],
//                                               folder: LeftFolder[O, PreparedStatement, prepareParameter.type],
//                                               from: From[T]
//                                              ): Seq[T] = {
//    val pstmt = new PreparedStatement
//    val params = statement.mkParameters(gen, ps)
//    println("DEBUG - sql : " + statement.parsedSql)
//    //TODO Filter is need ???
//    val pdstmt = folder(params, pstmt).asInstanceOf[PreparedStatement]
//    //TODO exe with pdstmt
//    println("DEBUG - pdstmt : " + pdstmt)
//    val rs = new ResultSet(3)
//    val seq1 = new ListBuffer[T]()
//    while (rs.next()) {
//      val x = from.to(rs)
//      seq1 += x
//    }
//    seq1.toList
//  }

  def getByValue[T, R <: HList, O <: HList](statement: Statement[T])
                                           (implicit gen: LabelledGeneric.Aux[T, R],
                                            ps: Parameters.Aux[R, O],
                                            folder: LeftFolder[O, PreparedStatement, prepareParameter.type]
                                           ): ResultSeq[T] = {
    if (statement.statType == StatementType.Procedure) {
      //???
    }
    val conn = getConnection
    try {
//      val pstmt = new PreparedStatement
      val pstmt = getPreparedStatement(conn, statement.parsedSql.toString, statement.statType, statement.autoId)
      val params = statement.mkParameters(gen, ps)
      println("DEBUG - sql : " + statement.parsedSql)
      //TODO Filter is need ???
      val pdstmt = folder(params, pstmt).asInstanceOf[PreparedStatement]
      println("DEBUG - pdstmt : " + pdstmt)
      val rs = pstmt.executeQuery()
//      val rs = new ResultSet(3)
//      pdstmt.close()
      new ResultSeq[T](rs, conn, pdstmt)
    } catch {
      case e: Exception => throw new RuntimeException(e)
    } finally {
//      conn.close()
    }

  }

  def update[T, R <: HList, O <: HList](statement: Statement[T])
                                       (implicit gen: LabelledGeneric.Aux[T, R],
                                        ps: Parameters.Aux[R, O],
                                        folder: Folder[O]
                                       ): ManipulationResult = {

    if(statement.statType == StatementType.Procedure) {
      //???
    }
    val conn = getConnection
    try {
//      val pstmt = new PreparedStatement
      val pstmt = getPreparedStatement(conn, statement.parsedSql.toString, statement.statType, statement.autoId)
      val params = statement.mkParameters(gen, ps)
      println("DEBUG - params: " + params)
      val pdstmt = folder(params, pstmt).asInstanceOf[PreparedStatement]
      val rowCount = pdstmt.executeUpdate()
      val autoId = if(statement.autoId) {
        val rs = pdstmt.getGeneratedKeys
        Some(if (rs.next()) rs.getInt(1) else -1)
      } else None
      pdstmt.close()
      ManipulationResult(rowCount, autoId)
    } catch {
      case e: Exception => throw new RuntimeException(e)
    } finally {
      conn.close()
    }

  }

  def updateBatch[T, R <: HList, O <: HList](statements: Statement[T]*)
                                            (implicit gen: LabelledGeneric.Aux[T, R],
                                             ps: Parameters.Aux[R, O],
                                             folder: Folder[O]
                                            ): Seq[ManipulationResult] = {
    val conn = getConnection
    try {
      var pstmtOpt: Option[PreparedStatement] = None
      statements.foreach { statement =>
        pstmtOpt match {
          case None =>
            val pstmt = getPreparedStatement(conn, statement.parsedSql.toString, statement.statType, statement.autoId)
//            val pstmt = new PreparedStatement
            pstmtOpt = Some(pstmt)
            val params = statement.mkParameters(gen, ps)
            folder(params, pstmt).asInstanceOf[PreparedStatement]
            pstmt.addBatch()
          case Some(pstmt) =>
            val params = statement.mkParameters(gen, ps)
            folder(params, pstmt).asInstanceOf[PreparedStatement]
            pstmt.addBatch()
        }
      }
      val resultOpt = pstmtOpt.map(_.executeBatch())
      val rsOpt = pstmtOpt.map(_.getGeneratedKeys())
      val res = new ListBuffer[ManipulationResult]()
      var i = 0
      val length = resultOpt.map(_.length).getOrElse(0)
      val result = if(length > 0) resultOpt.get else Array[Int]()
      while(i < length) {
        val key = rsOpt.map(rs => if (rs.next()) rs.getInt(1) else -1)
        res += ManipulationResult(result(i), key)
        i += 1
      }
      pstmtOpt.foreach(_.close())
      res.toSeq
    } catch {
      case e: Exception => throw new RuntimeException(e)
    } finally {
      conn.close()
    }

  }

  private def getPreparedStatement(conn: Connection, sql: String, stmtType: StatementType.Value, autoId: Boolean) =
    stmtType match {
      case StatementType.Sql =>
        if(autoId) conn.prepareStatement(sql, java.sql.Statement.RETURN_GENERATED_KEYS)
        else conn.prepareStatement(sql)
      case StatementType.Procedure => conn.prepareCall(sql)
      case _ => conn.prepareStatement(sql) //TODO error
    }
}


object TestExecutor {
  def main(args: Array[String]): Unit = {
    val sql = new StringBuilder("select * from user where id = #id and name = #name and age = #age")
    val stat = new Statement(sql, User(Option(1), "milo", 26), StatementType.Sql)

//    implicit def hcons[K <: Symbol, V, T <: HList, O1 <: HList, DBT <: DBType, O2 <: HList, Head, Tail <: HList]
//    (implicit wt: Witness.Aux[K],
//     mapper: Mapper.Aux[getFieldType.type, V :: HNil, O1],
//     isHCons: IsHCons.Aux[O1, DBT, HNil],
//     isHCons2: IsHCons.Aux[T, Head, Tail],
//     tParams: Parameters.Aux[T, O2]): Parameters.Aux[FieldType[K, V] :: T, Parameter[V, DBT#PolyF] :: O2] = Parameters.hcons(wt, mapper,isHCons, isHCons2, tParams)
//
//    implicit def hcons0[K <: Symbol, V, O1 <: HList, DBT <: DBType]
//    (implicit wt: Witness.Aux[K],
//     mapper: Mapper.Aux[getFieldType.type, V :: HNil, O1],
//     isHCons: IsHCons.Aux[O1, DBT, HNil]): Parameters.Aux[FieldType[K, V] :: HNil, Parameter[V, DBT#PolyF] :: HNil] = Parameters.hcons0(wt, mapper,isHCons)

    val executor = new MySqlExecutor("druid")
//    val r = executor.getByValue(stat)
//    println(r)
//    val r2 = new MySqlExecutor("druid").getOneByValue(stat)
//    println(r2)

//    val r3 = executor.getOneByValue(stat).as[(Int, String)]
//    println(r3)

    val sql2 = new StringBuilder("INSERT INTO user (name, age) VALUES (#name, #age )")
    val st = new Statement(sql2, User(None, "xxx", 23), StatementType.Sql, autoId = true)
    println(st.parsedSql, st.parametersIndexes, st.autoId)
    val r4 = executor.update(st)
    println(r4)

    java.sql.Date.valueOf("2017-04-15")
    //    val dj = new DataInject(new ResultSet1(3))
    //    val rx = dj.getData[User].toSeq
    //    println(rx)
//        val gen = LabelledGeneric[User]
//        val p2 = Parameters[gen.Repr]
//        val r2 = p2.parameters(gen.to(User(None, "a", 1)), stat.parametersIndexes)
//        println(r2)

  }
}