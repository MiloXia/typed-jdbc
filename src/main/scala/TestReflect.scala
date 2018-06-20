/**
  * Created by xiaziyao on 2017/4/10.
  */
object TestReflect {
  //reflect
  def getFieldMap[T: scala.reflect.runtime.universe.TypeTag]: Map[String, String] = {
    import scala.reflect.runtime.universe._
    val tpe = typeOf[T]
    //      println("class name: " + tpe.typeSymbol.name.toString)
    tpe.decls.collect {
      case m: MethodSymbol if m.isCaseAccessor => (m.name.toString, m.returnType.typeSymbol.name.toString)
    }.toMap
  }
  def getFieldMap2[T: scala.reflect.runtime.universe.TypeTag]: Map[String, String] = {
    import scala.reflect.runtime.universe._
    val tpe = typeOf[T]
    //      println("class name: " + tpe.typeSymbol.name.toString)
    val map1 = tpe.decls.collect {
      case m: TermSymbol if m.isVal || m.isVar => (m.name.toString, m.typeSignature.typeSymbol.name.toString)
    }.toMap
    val map2 = tpe.baseClasses.flatMap(sup => sup.typeSignature.decls).collect {
      case m: TermSymbol if m.isVal || m.isVar => (m.name.toString, m.typeSignature.typeSymbol.name.toString)
    }.toMap
    map1 ++ map2
  }
  def getScalaAnnotationsInClass[T: scala.reflect.runtime.universe.TypeTag]: Map[String, Map[String, String]] = {
    import scala.reflect.runtime.universe._
    val anns = typeOf[T].typeSymbol.annotations
    anns.map { ann: Annotation =>
      val tpe = ann.tree.tpe
      val annName = tpe.typeSymbol.name.toString
//      println(annName)
      val scalaArgs = ann.tree.children.tail
      val argMaps = scalaArgs.map { argTree =>
        val argVal = argTree.toString()
        val argName = argTree.tpe.typeSymbol.name.toString
        argVal -> argName
      }.toMap
      annName -> argMaps
    }.toMap
  }

  def getScalaAnnotationsInConstructor[T: scala.reflect.runtime.universe.TypeTag]: Map[String, Map[String, String]] = {
    import scala.reflect.runtime.universe._
    val anns = typeOf[T].decls.filter(_.isConstructor).flatMap(_.asMethod.paramLists.flatMap(_.flatMap(_.annotations)))
    anns.map { ann: Annotation =>
      val tpe = ann.tree.tpe
      val annName = tpe.typeSymbol.name.toString
//      println(annName)
      val scalaArgs = ann.tree.children.tail
      val argMaps = scalaArgs.map { argTree =>
        val argVal = argTree.toString()
        val argName = argTree.tpe.typeSymbol.name.toString
        argVal -> argName
      }.toMap
      annName -> argMaps
    }.toMap
  }

  class Super {
    var id = ""
  }
  case class Sub(x: Int) extends Super

  def main(args: Array[String]): Unit = {

    import scala.reflect.runtime.universe._
    val anns = typeOf[AA].typeSymbol.annotations
    anns.foreach{ ann: Annotation =>
      val tpe = ann.tree.tpe
      println(tpe.typeSymbol.name)
      val tree = ann.tree
//      println(showRaw(tree))
      tree.children.tail.foreach(t => println(t.toString))
//      println(show(tree.children.tail))
    }
    println(typeOf[AA].typeSymbol.annotations)
    println(getScalaAnnotationsInClass[AA])
    println("----------------")
    val anns2 = typeOf[AA].decls.collect {
      case m: MethodSymbol if m.isCaseAccessor => m.paramLists
    }
    println(anns2)
    println(typeOf[AA].decls.filter(_.isConstructor).flatMap(x => x.asMethod.paramLists.flatMap(x => x.flatMap(_.annotations))))
    println(getScalaAnnotationsInConstructor[AA])

    println(getFieldMap2[Sub])
  }
}
