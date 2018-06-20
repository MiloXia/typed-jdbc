package executor

import javax.sql.DataSource

import dbtype.{ByteType, IntType, UnknownType, Varchar}
import ds.ManipulationResult
import java.sql.{PreparedStatement, ResultSet}
import orm.JsaOrmFactory
import parameter.Parameter
import shapeless.labelled.FieldType
import shapeless.{::, Generic, HList, HNil, LabelledGeneric, Poly1, Poly2}
import shapeless.ops.hlist.{LeftFolder, Mapper}
import statement.{Parameters, Statement}

/**
  * Created by xiaziyao on 2017/4/14.
  */
trait SqlExecutor {
  import SqlExecutor._
  val poolName: String
  val dataSource: DataSource = JsaOrmFactory.getSqlExecutorAdapter(poolName).pool.dataSource
  def getConnection =
    try dataSource.getConnection()
    catch {
      case e: Exception => throw new RuntimeException(e)
    }

  //  def getOneByValue[T,  R <: HList, O <: HList](statement: Statement[T])(implicit gen: LabelledGeneric.Aux[T, R],
  //                                                                         ps: Parameters.Aux[R, O],
  //                                                                         folder: LeftFolder[O, PreparedStatement, prepareParameter.type],
  //                                                                         from: From[T]
  //  ): Option[T]

    //case class
    def getOneByValue[T, R <: HList, O <: HList](statement: Statement[T])
                                                (implicit gen: LabelledGeneric.Aux[T, R],
                                                 ps: Parameters.Aux[R, O],
                                                 folder: LeftFolder[O, PreparedStatement, prepareParameter.type]
                                                ): Result[T]

    //record tuple
    def getOneByValue2[T, R <: HList, O <: HList](statement: Statement[T])
                                                 (implicit gen: Generic.Aux[T, R],
                                                  ps: Parameters.Aux[R, O],
                                                  folder: LeftFolder[O, PreparedStatement, prepareParameter.type]
                                                 ): Result[T]
    //one field type
    def getOneByValue3[K, V, O <: HList](statement: Statement[FieldType[K, V]])
                                        (implicit
                                         ps: Parameters.Aux[(FieldType[K, V] :: HNil), O],
                                         folder: LeftFolder[O, PreparedStatement, prepareParameter.type]
                                        ): Result[V]

    //  def getByValue[T, R <: HList, O <: HList](statement: Statement[T])
    //                                           (implicit gen: LabelledGeneric.Aux[T, R],
    //                                            ps: Parameters.Aux[R, O],
    //                                            folder: LeftFolder[O, PreparedStatement, prepareParameter.type],
    //                                            from: From[T]
    //                                           ): Seq[T]
    def getByValue[T, R <: HList, O <: HList](statement: Statement[T])
                                             (implicit gen: LabelledGeneric.Aux[T, R],
                                              ps: Parameters.Aux[R, O],
                                              folder: LeftFolder[O, PreparedStatement, prepareParameter.type]
                                             ): ResultSeq[T]

    //same with getOneByValue
    def getById[T, R <: HList, O <: HList](statement: Statement[T])
                                          (implicit gen: LabelledGeneric.Aux[T, R],
                                           ps: Parameters.Aux[R, O],
                                           folder: LeftFolder[O, PreparedStatement, prepareParameter.type]
                                          ): Result[T] = getOneByValue(statement)(gen, ps, folder)

    def update[T, R <: HList, O <: HList](statement: Statement[T])
                                         (implicit gen: LabelledGeneric.Aux[T, R],
                                          ps: Parameters.Aux[R, O],
                                          folder: Folder[O]
                                         ): ManipulationResult

    def updateBatch[T, R <: HList, O <: HList](statements: Statement[T]*)
                                              (implicit gen: LabelledGeneric.Aux[T, R],
                                               ps: Parameters.Aux[R, O],
                                               folder: Folder[O]
                                              ): Seq[ManipulationResult]

  //TODO define other interface
}

object SqlExecutor {
  type Folder[O <: HList] = LeftFolder[O, PreparedStatement, prepareParameter.type]
}