import shapeless._
import shapeless.labelled.FieldType
import shapeless.ops.hlist.{FillWith, IsHCons, Mapper}
import shapeless.{HList, HNil, LabelledGeneric, Lazy, Witness}

/**
  * Created by xiaziyao on 2017/4/10.
  */
trait Fields[T] {
  val fields: Map[String, String]
}

object Fields {
  def apply[T](implicit f: Fields[T]) = f.fields

  //bridge
  implicit def genericFields[T, R <: HList]
    (implicit gen: LabelledGeneric.Aux[T, R], //to HList
              pf: Fields[R]): Fields[T] =
    new Fields[T] {
      override val fields = pf.fields
    }
  //HNil
  implicit val hnilFields: Fields[HNil] = new Fields[HNil] {
    override val fields = Map() //HNil iso List()
  }
  //FieldType[K, V] :: T
//  import scala.reflect.runtime.{universe => ru}
//  implicit def hlistFields[K <: Symbol, V, T <: HList]
//    (implicit wt: Witness.Aux[K],
//              tg: ru.TypeTag[V],
//              tFields: Fields[T]) =
//    new Fields[FieldType[K, V] :: T] {
//      override def fields = wt.value.name + " : " + tg.tpe.toString :: tFields.fields
//    }
  implicit def hlistFields[K <: Symbol, V, T <: HList, O <: HList, O1 <: HList]
  (implicit wt: Witness.Aux[K],
            fillw: FillWith[fillField.type, V :: HNil] {type Out = O},
            mapper: Mapper.Aux[getFieldType.type, O, O1],
            isHCons: IsHCons.Aux[O1, String, HNil],
            tFields: Fields[T]) =
    new Fields[FieldType[K, V] :: T] {
      val init = fillw.apply()
      val fieldType: String = mapper.apply(init).head
      override val fields =  tFields.fields + (wt.value.name  ->  fieldType)
    }

  object getFieldType extends shapeless.Poly1 { //or return Int (type encode/const)
    implicit def caseInt: Case.Aux[Int, String] = at(_ => "Int")
    implicit def caseString: Case.Aux[String, String] = at(_ => "String")
  }

  object fillField extends shapeless.Poly0 {
    implicit def caseInt = at[Int](0)
    implicit def caseString = at[String]("")
  }

}

class Ann(val name: String, val id: Int) extends scala.annotation.StaticAnnotation
class Ann2(val name: String) extends scala.annotation.StaticAnnotation
case class A()
@Ann("hello", id = 1)
@Ann2("hello2")
case class AA(@Ann2("hello3") i: Int)

object TestGetFields {

  def main(args: Array[String]): Unit = {
    println("get start"); val start = System.nanoTime()
    val fields = Fields[User]
    val fields2 = Fields[A]
    val fields3 = Fields[AA]
    println("get end"); val end = System.nanoTime()
    println(fields, fields2, fields3)
    println((end - start) / 1000000L)

  }
}
