// abstract class Tree
// case class Leaf(v:Int) extends Tree
// case class Node(v:Int, l:Tree, r:Tree) extends Tree

abstract class Tree[+T]
case class Leaf[T](v:T) extends Tree[T]
case class Node[T](v:T, l:Tree[T], r:Tree[T]) extends Tree[T]

// abstract class Tree[T <: Ordered[T]]
// case class Leaf[T <: Ordered[T]](v:T) extends Tree[T]
// case class Node[T <: Ordered[T]](v:T, l:Tree[T], r:Tree[T]) extends Tree[T]

// abstract class Tree[T <: Ordered[T]]
//     case class Node[T <: Ordered[T]](v:T, l:Tree[T], r:Tree[T]) extends Tree[T]
//     case class Leaf[T <: Ordered[T]](v:T) extends Tree[T]

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
        // println("hello world")
        // val p = new A(5)
        // println(p)
        // val p2 = new B(7)
        // println(p2)
        // val p3 = new C(9)
        // println(p3)
        // val p4 = new A(10) + new A(8)
        // println(p4)
        // val p5 = new B(100) + new B(200)
        // println(p5)
        // val p7 = new C(1000) + new C(2000)
        // println(p7)

        val myBTree: Tree[B] = Node(new B(4),Node(new B(2),Leaf(new B(1)),Leaf(new B(3))), 
                           Node(new B(6), Leaf(new B(5)), Leaf(new B(7))))
        println(myBTree)

        val myATree: Tree[A] = myBTree
        // println(myATree)

        println("inOrder = " + inOrder(myATree))
        println("Sum = " + treeSum(myATree))

        // println(treeSum(myBTree))

        def faa(a:A):A = new A(a.value+10)
        def fab(a:A):B = new B(a.value+20)
        def fba(b:B):A = new A(b.value+30)
        def fbb(b:B):B = new B(b.value+40)
        def fbc(b:B):C = new C(b.value+50)
        def fcb(c:C):B = new B(c.value+60)
        def fcc(c:C):C = new C(c.value+70)
        def fac(a:A):C = new C(a.value+80)
        def fca(c:C):A = new A(c.value+90)


        // println(BTreeMap(faa,myBTree))
        println(BTreeMap(fab,myBTree))
        // println(BTreeMap(fba,myBTree))
        println(BTreeMap(fbb,myBTree))
        println(BTreeMap(fbc,myBTree))
        // println(BTreeMap(fcb,myBTree))
        // println(BTreeMap(fcc,myBTree))
        println(BTreeMap(fac,myBTree))
        // println(BTreeMap(fca,myBTree))


        println(treeMap(faa,myATree))
        println(treeMap(fab,myATree))
        // println(treeMap(fba,myATree))
        // println(treeMap(fbb,myATree))
        // println(treeMap(fbc,myATree))
        // println(treeMap(fcb,myATree))
        // println(treeMap(fcc,myATree))
        println(treeMap(fac,myATree))
        // println(treeMap(fca,myATree))
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
    def treeMap[T](myfunc: T=>T, t:Tree[T]):Tree[T]={
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
    // def treeSum[T <: Addable[T]]( t:Tree[T]):T
    // def test():Unit = {
    //     def faa(a:A):A = new A(a.value+10)
    //     def fab(a:A):B = new B(a.value+20)
    //     def fba(b:B):A = new A(b.value+30)
    //     def fbb(b:B):B = new B(b.value+40)
    //     def fbc(b:B):C = new C(b.value+50)
    //     def fcb(c:C):B = new B(c.value+60)
    //     def fcc(c:C):C = new C(c.value+70)
    //     def fac(a:A):C = new C(a.value+80)
    //     def fca(c:C):A = new A(c.value+90)

        // val myBTree: Tree[B] = Node(new B(4),Node(new B(2),Leaf(new B(1)),Leaf(new B(3))), 
        //                    Node(new B(6), Leaf(new B(5)), Leaf(new B(7))))

    //     val myATree: Tree[A] = myBTree

    //     println("inOrder = " + inOrder(myATree))
    //     println("Sum = " + treeSum(myATree))

    //     println(BTreeMap(faa,myBTree))
    //     println(BTreeMap(fab,myBTree))
    //     println(BTreeMap(fba,myBTree))
    //     println(BTreeMap(fbb,myBTree))
    //     println(BTreeMap(fbc,myBTree))
    //     println(BTreeMap(fcb,myBTree))
    //     println(BTreeMap(fcc,myBTree))
    //     println(BTreeMap(fac,myBTree))
    //     println(BTreeMap(fca,myBTree))

    //     println(treeMap(faa,myATree))
    //     println(treeMap(fab,myATree))
    //     println(treeMap(fba,myATree))
    //     println(treeMap(fbb,myATree))
    //     println(treeMap(fbc,myATree))
    //     println(treeMap(fcb,myATree))
    //     println(treeMap(fcc,myATree))
    //     println(treeMap(fac,myATree))
    //     println(treeMap(fca,myATree))
    // }
}

