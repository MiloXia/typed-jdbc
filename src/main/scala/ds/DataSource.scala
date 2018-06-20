package ds

import annotation.{AutoId, EntityMaps}
import executor._
import java.sql.PreparedStatement

import macrot.GetTypeName
import macrot.RecordSyntax.Record
import orm.JsaOrmFactory
import shapeless.{::, Generic, HList, HNil, LabelledGeneric, Witness}
import shapeless.labelled.FieldType
import shapeless.ops.hlist.{LeftFolder, Mapper, ToTraversable}
import statement.{Parameters, Statement, StatementType}
import vo.User

import scala.annotation.switch

/**
  * Created by xiaziyao on 2017/4/18.
  */
trait JsaDataSource {
  val dbRef: String
  val executor: SqlExecutor = JsaOrmFactory.getSqlExecutorAdapter(dbRef).sqlExecutor
}
class DataSource(val dbRef: String) extends JsaDataSource {

//  def getByValue[T,  R <: HList, O <: HList](vo: T, params: String*)
//                                            (implicit gen: LabelledGeneric.Aux[T, R],
//                                             ps: Parameters.Aux[R, O],
//                                             folder: LeftFolder[O, PreparedStatement, prepareParameter.type],
//                                             from: From[T]
//  ): Seq[T] = {
//    //getQuery
//    val sql = new StringBuffer("select * from user where id = #id, name = #name, age = #age")
//    val stat = new Statement(sql, vo, StatementType.Sql)
//    val r = executor.getByValue(stat)
//    r
//  }

//  def getOneByValue[T,  R <: HList, O <: HList](vo: T, params: String*)
//                                            (implicit gen: LabelledGeneric.Aux[T, R],
//                                             ps: Parameters.Aux[R, O],
//                                             folder: LeftFolder[O, PreparedStatement, prepareParameter.type],
//                                             from: From[T]
//                                            ): Option[T] = {
//    //getQuery
//    val sql = new StringBuffer("select * from user where id = #id, age = #age")
//    val stat = new Statement(sql, vo, StatementType.Sql)
//    val r = executor.getOneByValue(stat)
//    r
//  }

  def getOneByValue2[T,  R <: HList, O <: HList](vo: T, params: String*)
                                               (implicit gen: Generic.Aux[T, R],
                                                ps: Parameters.Aux[R, O],
                                                folder: LeftFolder[O, PreparedStatement, prepareParameter.type]
                                               ): Result[T] = {
    //getQuery
    val sql = new StringBuilder("select * from user where id = #id and age = #age")
    val stat = new Statement(sql, vo, StatementType.Sql)
    val r = executor.getOneByValue2(stat)
    r
  }

  def getOneByValue[T, R <: HList, R2 <: HList, O <: HList](vo: Record[T])
                                                (implicit gen: Generic.Aux[T, R],
                                                 mapper: Mapper.Aux[getRecordFieldName.type, R, R2],
                                                 traversable: ToTraversable.Aux[R2, scala.List, String],
                                                 ps: Parameters.Aux[R, O],
                                                 folder: LeftFolder[O, PreparedStatement, prepareParameter.type]
                                                ): Result[T] = {
    val hlist = gen.to(vo.rec)
    val params: List[String] = hlist.map(getRecordFieldName).toList
    println(params)
    val sql = getQuery(vo.tableName, SqlType.Select, params.map(_.replaceFirst("'", "")))
    println(sql)
//    val sql = new StringBuilder("select * from user where id = #id and age = #age")
    val stat = new Statement(sql, vo.rec, StatementType.Sql)
    val r = executor.getOneByValue2(stat)
    r
  }

  object getRecordFieldName extends shapeless.Poly1 {
    implicit def getFileName[K, V](implicit witness: Witness.Aux[K]): Case.Aux[FieldType[K, V], String] = at(_ => witness.value.toString)
  }

  def getOneByValue3[K, V, O <: HList](vo: FieldType[K, V], params: String*)
                                                (implicit
                                                 ps: Parameters.Aux[(FieldType[K, V] :: HNil), O],
                                                 folder: LeftFolder[O, PreparedStatement, prepareParameter.type]
                                                ): Result[V] = {
    //getQuery
    val sql = new StringBuilder("select * from user where id = #id")
    val stat = new Statement(sql, vo, StatementType.Sql)
    val r = executor.getOneByValue3(stat)
    r
  }

  private def getQuery[T](tableName: String, sqlType: SqlType.Value, fields: List[String]): StringBuilder = {
    import SqlType._
    val sql = new StringBuilder("")
    sqlType match {
      case Select =>
        sql.append("select * from ").append(tableName)
        if(fields.nonEmpty) {
          sql.append(" where ")
          val where = fields.map(field => new StringBuilder(field).append(" = ").append(Statement.separate).append(field).toString()).mkString(" and ")
          sql.append(where)
        }
        sql
      case _ =>  ??? //TODO
    }
  }

  private def getQuery[T](vo: T, tableName: String, sqlType: SqlType.Value, fields: List[String]): Statement[T] = {
    import SqlType._
    val sql = new StringBuilder("")
    val entityOpt = EntityMaps.getEntity(tableName)
    //get Table by reflect
    sqlType match {
      case Insert => //sql.append
        val autoId = entityOpt.get.fields.flatMap(_.getAnnotation(classOf[AutoId])).headOption
        if(autoId.nonEmpty) new Statement(sql, vo, autoId = true)
        else new Statement(sql, vo, autoId = false)
      case Update => ???
      case _ =>  ??? //TODO
    }
  }

}

object TestDS {
  def main(args: Array[String]): Unit = {
    //    println(getByValue(User(Option(1), "aa", 20)))
    //    println(getOneByValue(User(Option(1), "aax", 20)))
    import macrot.RecordSyntax.key._
    import macrot.RecordSyntax.vo
    //    val param = "id" ->> 2 :: HNil
    //    def f[T, R <: HList](lab: T)(implicit gen: Generic.Aux[T, R], param: Parameters[R]) = gen.to(lab)
    //
    //    def f2[T <: HList](lab: T)(implicit param: Parameters[T]) = param
    //    println(f2(param))
    val ds = JsaOrmFactory.getSqlExecutorAdapter("druid").dataSource

    val user1 = ds.getOneByValue2(('id ->> Option(1), 'age ->> 26)).as[User]
    println(user1)
    println("--------4")
    val id: Option[Int] = Some(1)
    println(ds.getOneByValue(vo[User]('id ->> id, 'age ->> user1.map(_.age).getOrElse(26))).as[User])
    println("--------3")
    println(ds.getOneByValue3('id ->> Option(1)).as[User])
  }
}



object SqlType extends Enumeration {
  val Insert, Update, Delete, Select = Value
}
