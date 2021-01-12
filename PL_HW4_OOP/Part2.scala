// Chi-Yu Lin N16594213
// Programming Language HW4 OOP

abstract class Tree[+T]
case class Leaf[T](v:T) extends Tree[T]
case class Node[T](v:T, l:Tree[T], r:Tree[T]) extends Tree[T]

//-------------------------------------------------------
trait Addable[T]{
    def +(that: T): T
}
class A(var value:Int) extends Addable[A]{
    def +(that: A) = new A(this.value + that.value)
    override def toString(): String = s"A($value)"
}
class B(value:Int) extends A(value){
    def +(that: B) = new B(this.value + that.value)
    override def toString(): String = s"B($value)"
}
class C(value:Int) extends B(value){
    def +(that: C) = new C(this.value + that.value)
    override def toString(): String = s"C($value)"
}
//-------------------------------------------------------

object Part2{

    def main(args: Array[String]) = {
        test()
    }
    def inOrder[T](t: Tree[T]):List[T]={
        t match {
            case Leaf(v) => List(v)
            case Node(v,l,r) => inOrder(l) ++ List(v) ++ inOrder(r)
        }
    }
    def treeSum[T <: Addable[T]](t: Tree[T]):T={
        t match {
            case Leaf(v) => v
            case Node(v,l,r) => ( (treeSum(l) + v ) + treeSum(r))
        }
    }
    def treeMap[T,V](myfunc: T=>V, t:Tree[T]):Tree[V]={
        t match {
            case Leaf(v) => Leaf(myfunc(v))
            case Node(v,l,r) => Node(myfunc(v), treeMap(myfunc, l), treeMap(myfunc, r))
        }
    }
    def BTreeMap(myfunc: B=>B, t:Tree[B]):Tree[B]={
        t match {
            case Leaf(v) => Leaf(myfunc(v))
            case Node(v,l,r) => Node(myfunc(v), treeMap(myfunc, l), treeMap(myfunc, r))
        }
    }
    def test():Unit = {
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

        // println(BTreeMap(faa,myBTree))   //error: cannot store an A into B
        println(BTreeMap(fab,myBTree))
        // println(BTreeMap(fba,myBTree))   //error: cannot store an A into B
        println(BTreeMap(fbb,myBTree))
        println(BTreeMap(fbc,myBTree))
        // println(BTreeMap(fcb,myBTree))   //error: cannot have B as input in fcb
        // println(BTreeMap(fcc,myBTree))   //error: cannot have B as input in fcc
        println(BTreeMap(fac,myBTree))
        // println(BTreeMap(fca,myBTree))   //error: cannot store an A into B

        println(treeMap(faa,myATree))
        println(treeMap(fab,myATree))
        // println(treeMap(fba,myATree))    //error: cannot have B as input of type A
        // println(treeMap(fbb,myATree))    //error: cannot have B as input of type A
        // println(treeMap(fbc,myATree))    //error: cannot have B as input of type A
        // println(treeMap(fcb,myATree))    //error: cannot have C as input of type A
        // println(treeMap(fcc,myATree))    //error: cannot have C as input of type A
        println(treeMap(fac,myATree))
        // println(treeMap(fca,myATree))    //error: cannot have C as input of type A
    }
}
