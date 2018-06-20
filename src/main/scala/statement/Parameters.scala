package statement

import dbtype._
import parameter.Parameter
import shapeless.{::, Generic, HList, HNil, LabelledGeneric, Witness}
import shapeless.labelled.FieldType
import shapeless.ops.hlist.{IsHCons, Mapper}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
  * Created by xiaziyao on 2017/4/18.
  */

//@implicitNotFound(s"can't get parameters for type ${L}")
trait Parameters[L <: HList] {
  type Out <: HList
  def parameters(l: L, paramIndexes: mutable.Map[String, ListBuffer[Int]]): Out
}

object Parameters {
  def apply[R <: HList](implicit p: Parameters[R]): Aux[R, p.Out] = p

  def make[T, R <: HList](vo: T, paramIndexes: mutable.Map[String, ListBuffer[Int]])
                         (implicit gen: LabelledGeneric.Aux[T, R], p: Parameters[R]): p.Out = {
    val r = gen.to(vo)
    //    println(r)
    p.parameters(r, paramIndexes)
  }

  def make2[T, R <: HList](vo: T, paramIndexes: mutable.Map[String, ListBuffer[Int]])
                         (implicit gen: Generic.Aux[T, R], p: Parameters[R]): p.Out = {
    val r = gen.to(vo)
//        println(r)
    p.parameters(r, paramIndexes)
  }

  def make3[T](vo: T, paramIndexes: mutable.Map[String, ListBuffer[Int]])
                          (implicit p: Parameters[(T :: HNil)]): p.Out = {
//    val r = gen.to(vo)
    //        println(r)
    p.parameters(vo :: HNil, paramIndexes)
  }

  type Aux[T <: HList, O <: HList] = Parameters[T] { type Out = O }

  //HNil
  //  implicit val hnil: Parameters[HNil] = new Parameters[HNil] {
  //    type Out = HNil
  //    override def parameters(l : HNil, paramIndexes: mutable.Map[String, ListBuffer[Int]]): HNil = HNil
  //  }

  //H :: HNil
  implicit def hcons0[K <: Symbol, V, O1 <: HList, DBT <: DBType]
  (implicit wt: Witness.Aux[K],
   mapper: Mapper.Aux[getFieldType.type, V :: HNil, O1],
   isHCons: IsHCons.Aux[O1, DBT, HNil]): Aux[FieldType[K, V] :: HNil, Parameter[V, DBT#PolyF] :: HNil] =
    new Parameters[FieldType[K, V] :: HNil] {
      type Out = Parameter[V, DBT#PolyF] :: HNil
      override def parameters(l : FieldType[K, V] :: HNil, paramIndexes: mutable.Map[String, ListBuffer[Int]]) = {
        val fieldType = mapper(l.head :: HNil).head
        val fieldName = wt.value.name
        val indexesOpt = paramIndexes.get(fieldName)
        if(indexesOpt.isEmpty){
          //          throw new NoSuchElementException(s"no field '$fieldName' in sql parameters")
          println(s"no field '$fieldName' in sql parameters")
          new Parameter[V, DBT#PolyF](fieldName, l.head, List(), fieldType) :: HNil
          //TODO OP
        } else {
          new Parameter[V, DBT#PolyF](fieldName, l.head, indexesOpt.get.toList, fieldType) :: HNil
        }
      }
    }

  //  implicit def hcons[K <: Symbol, V, T <: HList, O <: HList, O1 <: HList, O2 <: HList]
  //    (implicit wt: Witness.Aux[K],
  //              fillw: FillWith[fillField.type, V :: HNil] {type Out = O},
  //              mapper: Mapper.Aux[getFieldType.type, O, O1],
  //              isHCons: IsHCons.Aux[O1, Int, HNil],
  //              tParams: Parameters.Aux[T, O2]) =
  //    new Parameters[FieldType[K, V] :: T] {
  //      val init = fillw.apply()
  //      val fieldType: Int = mapper.apply(init).head
  //
  //      type Out = Parameter[V] :: O2
  //      override def parameters(l : FieldType[K, V] :: T) = {
  //        new Parameter[V](wt.value.name, fieldType, l.head, Seq()) :: tParams.parameters(l.tail)
  //      }
  //    }
  implicit def hcons[K <: Symbol, V, T <: HList, O1 <: HList, DBT <: DBType, O2 <: HList, Head, Tail <: HList]
  (implicit wt: Witness.Aux[K],
   mapper: Mapper.Aux[getFieldType.type, V :: HNil, O1],
   isHCons: IsHCons.Aux[O1, DBT, HNil],
   isHCons2: IsHCons.Aux[T, Head, Tail],
   tParams: Parameters.Aux[T, O2]): Aux[FieldType[K, V] :: T, Parameter[V, DBT#PolyF] :: O2] =
  new Parameters[FieldType[K, V] :: T] {
    type Out = Parameter[V, DBT#PolyF] :: O2
    override def parameters(l : FieldType[K, V] :: T, paramIndexes: mutable.Map[String, ListBuffer[Int]]) = {
      val fieldType = mapper(l.head :: HNil).head
      val fieldName = wt.value.name
      val indexesOpt = paramIndexes.get(fieldName)
      if(indexesOpt.isEmpty) {
        //        throw new NoSuchElementException(s"no field '$fieldName' in sql parameters")
        new Parameter[V, DBT#PolyF](fieldName, l.head, List(), fieldType) :: tParams.parameters(l.tail, paramIndexes)
        //TODO OP
      } else {
        new Parameter[V, DBT#PolyF](fieldName, l.head, indexesOpt.get.toList, fieldType) :: tParams.parameters(l.tail, paramIndexes)
      }
    }
  }
}

object TestParam {
  def main(args: Array[String]): Unit = {
    import shapeless._, labelled._, record._, syntax.singleton._
    val param = 'id ->> 2 :: HNil
    def f[T, R <: HList](lab: T)(implicit gen: Generic.Aux[T, R], param: Parameters[R]) = gen.to(lab)

    def f2[T <: HList](lab: T)(implicit param: Parameters[T]) = param
    case class A(id: Int)
//    def f3[T, R <: HList](lab: T)(implicit gen: LabelledGeneric.Aux[T, R]) = {
//      Parameters[R]
//    }
    println(f2(param))
//    println(f3(A(3)))
    val iw = Witness('id)
    type Repr = FieldType[iw.T, Int] :: HNil
    Parameters[Repr]
  }
}