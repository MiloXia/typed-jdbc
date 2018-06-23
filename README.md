# typed-jdbc

This is a JDBC library, I use typeclass `Inject[T]` to generate ordinary JDBC code (in compile), the core idea is automatically deriving type class instances with shapeless library.
So it power is let us write ordinary JDBC code without boilerplate code.

for example, you have table as follow:

```sql
create table user(
    id int not null auto_increment,
    name varchar(8),
    age int,
    primary key(id));
```

and the vo class (or whatever you want mapping):

```scala
case class User(id: Option[Int], name: String, age: Int)
```

you can use such as:

```scala
object TestDS {

  def main(args: Array[String]): Unit = {
    import macrot.RecordSyntax.key._
    import macrot.RecordSyntax.vo

    val ds = DBFactory.getSqlExecutorAdapter("druid").dataSource

    val user: Option[User] = ds.getOneByValue(vo[User]('id ->> Option(1), 'age ->> 26)).as[User]
    println(user)

  }
}
```
the `vo[User]('id ->> Option(1), 'age ->> 26)` is the query params, the `as[User]` is map to case class.

<br />
I want more features, so this library is not release.

#### the core idea:

```scala
class TestPoly {

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
    private var _rs: ResultSet = _
    def setResult(rs: ResultSet) = _rs = rs
    implicit def readInt: Case.Aux[(Int, Int), Int] = at(i => _rs.getInt(i._2))
    implicit def readString: Case.Aux[(String, Int), String] = at(i => _rs.getString(i._2))
  }

  object fillField extends shapeless.Poly0 {
    implicit def caseInt = at[Int](0)
    implicit def caseString = at[String]("str")
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
case class Address(id: Int, city: String)

object PolyTest {
  val p = new TestPoly
  val p2 = new TestPoly

  def main(args: Array[String]) {
    println("--------------")
    val rs = new ResultSet(3)
    println(p.getData[User](rs))
    println("--------------")
    val rs2 = new ResultSet(3)
    println(p2.getData[Address](rs2))
    println("--------------")
    val rs3 = new ResultSet(2)
    println(p.getData[(Int, Int)](rs3))
  }
}
```

In this library I remove the variable (like: _i, _rs), make it more functional.