/**
  * Created by MiloXia on 2017/4/8.
  */
import shapeless._
import shapeless.ops.hlist.{Transposer, IsHCons, Zip, FillWith, Mapper}

import scala.collection.mutable.ListBuffer

class TestPoly {

//  object myPoly extends Poly1 {
//    implicit val intCase: Case.Aux[Int, Double] =
//      at(num => num / 2.0)
//    implicit val stringCase: Case.Aux[String, Int] =
//      at(str => str.length)
//  }
//
//  object multiply extends Poly2 {
//    implicit val intIntCase: Case.Aux[Int, Int, Int] =
//      at((a, b) => a * b)
//    implicit val intStrCase: Case.Aux[Int, String, String] =
//      at((a, b) => b.toString * a)
//  }
//
  object sizeOf extends Poly1 {
    implicit val intCase: Case.Aux[Int, Int] =
      at(identity)
    implicit val stringCase: Case.Aux[String, Int] =
      at(_.length)
    implicit val booleanCase: Case.Aux[Boolean, Int] =
      at(bool => if(bool) 1 else 0)
  }

//  val s = (10 :: "hello" :: true :: HNil).map(sizeOf)

  object mkIndex extends Poly1 {
    private var _i = 0
    def rest(): Unit = _i = 0
    implicit val intCase: Case.Aux[Int, Int] =
      at(_ => {_i += 1; _i})
    implicit val stringCase: Case.Aux[String, Int] =
      at(_ => {_i += 1; _i})
    implicit val booleanCase: Case.Aux[Boolean, Int] =
      at(_ => {_i += 1; _i})
  }


  object readField extends shapeless.Poly1 {
    private var _rs: ResultSet = _ //TODO Option
    def setResult(rs: ResultSet) = _rs = rs
    implicit def readInt: Case.Aux[(Int, Int), Int] = at(i => _rs.getInt(i._2))
    implicit def readString: Case.Aux[(String, Int), String] = at(i => _rs.getString(i._2))
    implicit def readOption[T](implicit rd: Case.Aux[(T, Int), T]) = at[(Option[T], Int)](i =>
      {readField((i._1.get, i._2))}
    )
  }

  object fillField extends shapeless.Poly0 {
    implicit def caseInt = at[Int](0)
    implicit def caseString = at[String]("str")
    implicit def caseOption[T](implicit c: Case0[T]) = at[Option[T]](Some(fillField.apply[T]))
  }

  trait Inject[T] {
    def data: T
  }
  object Inject {
    def apply[T](implicit inject: Inject[T]) = inject

    implicit def fill[T, R <: HList, O <: HList, O1 <: HList, O2 <: HList, R1 <: HList]
      (implicit gen: Generic.Aux[T, R],
               fillw: FillWith[fillField.type, R] {type Out = O},
               mapper: Mapper.Aux[mkIndex.type, O, O1],
               zipper : Zip.Aux[O :: O1 :: HNil, O2],
               mapper2: Mapper.Aux[readField.type, O2, R1],
               gen2: Generic.Aux[T, R1]) = {
      println("debug - run")
      val init = fillw.apply()
      mkIndex.rest() //MUST
      val index = mapper.apply(init)
      val zipped = zipper.apply(init :: index :: HNil)
      new Inject[T] {
        def data = {
          val r = mapper2.apply(zipped)
          gen2.from(r)
        }
      }
    }
  }

  def getData[T](rs: ResultSet)(implicit inject: Inject[T]) = {
    readField.setResult(rs)
    val seq1 = new ListBuffer[T]()
    while (rs.next()) {
      val x = inject.data
      seq1 += x
    }
    seq1
  }
}

class ResultSet(size: Int) {
  var rest = size
  def next() : Boolean = {
    val r = rest > 0
    if(r) rest -= 1
    r
  }
  def getInt(i: Int) = i
  def getString(i: Int) = "str_" + i.toString
  //...
}

case class User(id: Int, name: String, age: Int)
//  type U = Int :: String :: HNil
case class Address(id: Option[Int], city: String)

object fillField2 extends shapeless.Poly0 {
  implicit val caseInt = at[Int](0)
  implicit val caseString = at[String]("str")
  implicit def caseOption[T](implicit c: Case0[T]) = at[Option[T]](Some(fillField2.apply[T]))
}

object PolyTest {
  val p = new TestPoly
  val p2 = new TestPoly
//  implicitly[p.readField.type =:= p2.readField.type]
  def main(args: Array[String]) {
    //    println((new myPoly).apply(1))
    //    println(multiply(2, "2"))

    //    println(s)

    println("--------------")
    val rs = new ResultSet(3)
    //    readField.setResult(rs)
    //    var seq1 = new ListBuffer[User]()
    //    while (rs.next()) {
    //      val x = Inject[User]
    //      seq1 += x
    //    }
    //    println(seq1)
    println(p.getData[User](rs))
    println("--------------")
    val rs2 = new ResultSet(3)
    //    readField.setResult(rs2)
    //    var seq2 = new ListBuffer[Address]()
    //    while (rs2.next()) {
    //      val x = Inject[Address].data
    //      seq2 += x
    //    }
//    println(p2.getData[Address](rs2))
    println("--------------")
    val rs3 = new ResultSet(2)
    println(p.getData[(Int, Int)](rs3))
    //    val gen = Generic[User]
    //    val hlist: U = HList.fillWith[gen.Repr](fillField)
    //
    ////    println(hlist.map(sizeOf2))
    //    sizeOf2.rest()
    //
    //    val indexl = hlist.map(sizeOf2)
    //    val h2 = hlist.zip(indexl)
    //    println(h2)
    //
    //    var seq = new ListBuffer[User]()
    //    readField.setResult(rs)
    //    while (rs.next()) {
    //      val rr = h2.map(readField)
    //      println(rr)
    //      val x = Generic[User].from(rr)
    //      println(x)
    //      seq += x
    //    }
    //    println(seq)

//    val lg = LabelledGeneric[User]
//    HList.fillWith[lg.Repr](fiilFieldType)
      val gen = Generic[Address]
      implicitly[(Option[Int] :: String :: HNil) =:= gen.Repr]
      val hlist = HList.fillWith[gen.Repr](fillField2)
    println(hlist)
  }
}
//class A extends scala.annotation.StaticAnnotation
//class B extends scala.annotation.ClassfileAnnotation

