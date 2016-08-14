abstract class Tree[+T]
case class Node[+T](label:T,left:Tree[T],right:Tree[T]) extends Tree[T]
case class Leaf[+T](label:T) extends Tree[T]

trait Addable[T] { 
   def +(x:T): T 
}

class A (x:Int) extends Addable[A] {
   val value = x
   def +(oth:A):A = new A(value + oth.value)
   override def toString() = "A(" + value +")"
}

class B (x:Int) extends A(x) {
   override def toString() = "B(" + value +")"
}

class C (x:Int) extends B(x) {
   override def toString() = "C(" + value +")"
}


object Part2 {

   def inOrder[T] (t:Tree[T]):List[T] = t match {
      case Leaf(x) => List(x)
      case Node(x,left,right) =>  inOrder(left) ++ List(x) ++ inOrder(right)
   }

   def treeSum[T <: Addable[T]] (t:Tree[T]) : T = t match {
      case Leaf(x) => x
      case Node(x,left,right) => (x + (treeSum(left))) + (treeSum(right))
   }

   def treeMap[T,V] (f:T=>V, t:Tree[T]):Tree[V] = t match {
      case Leaf (x) => Leaf(f(x))
      case Node(x,left,right) => Node(f(x), treeMap(f,left), treeMap(f,right))
   }

   def BTreeMap (f:B=>B, t:Tree[B]):Tree[B] = t match {
      case Leaf(x) => Leaf(f(x))
      case Node(x, left, right) => Node(f(x), BTreeMap(f,left) , BTreeMap(f,right) )
   }
 

   def test() {
      def faa(a:A):A = new A(a.value+10)
      def fab(a:A):B = new B(a.value+20)
      def fba(b:B):A = new A(b.value+30)
      def fbb(b:B):B = new B(b.value+40)
      def fbc(b:B):C = new C(b.value+50)
      def fcb(c:C):B = new B(c.value+60)
      def fcc(c:C):C = new C(c.value+70)
      def fac(a:A):C = new C(a.value+80)
      def fca(c:C):A = new A(c.value+90)

      val myBTree: Tree[B] = Node(new B(4),Node(new B(2),Leaf(new B(1)),Leaf(new B(3))), 
                     Node(new B(6), Leaf(new B(5)), Leaf(new B(7))))

      val myATree: Tree[A] = myBTree

      println("inOrder = " + inOrder(myATree))
      println("Sum = " + treeSum(myATree))

      //println(BTreeMap(faa,myBTree))   A=>A is not <: B=>B as function subtyping is covariant on return type
      println(BTreeMap(fab,myBTree))
      //println(BTreeMap(fba,myBTree))   B=>A is not <: B=>B as function subtyping is covariant on return type
      println(BTreeMap(fbb,myBTree))
      println(BTreeMap(fbc,myBTree))
      //println(BTreeMap(fcb,myBTree))   C=>B is not <: B=>B as function subtyping is contravariant on input type
      //println(BTreeMap(fcc,myBTree))   C=>C is not <: B=>B as function subtyping is contravariant on input type 
      println(BTreeMap(fac,myBTree))
      //println(BTreeMap(fca,myBTree))   C=>A is not <: B=>B as function subtyping is covariant on return type and contravariant on input type

      println(treeMap(faa,myATree))
      println(treeMap(fab,myATree))
      //println(treeMap(fba,myATree))  B=>A is not <: A=>A as function subtyping is contravariant on input type 
      //println(treeMap(fbb,myATree))  B=>B is not <: A=>B as function subtyping is contravariant on input type
      //println(treeMap(fbc,myATree))  B=>C is not <: A=>C as function subtyping is contravariant on input type
      //println(treeMap(fcb,myATree))  C=>B is not <: A=>B as function subtyping is contravariant on input type
      //println(treeMap(fcc,myATree))  C=>C is not <: A=>C as function subtyping is contravariant on input type
      println(treeMap(fac,myATree))
      //println(treeMap(fca,myATree))  C=>A is not <: A=>A as function subtyping is contravariant on input type
   }

   def main(args: Array[String]) {
      test();
   }
}
