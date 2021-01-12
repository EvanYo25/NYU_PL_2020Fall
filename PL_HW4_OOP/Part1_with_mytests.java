// Chi-Yu Lin N16594213
// Programming Language HW4 OOP

import java.util.*;

class SortedList<T extends Comparable<T>> extends ArrayList<T>{

	public String toString(){
		String ret = "";
		ret += "[[";
		for(int i=0 ; i<this.size() ; i++){
			if(i==this.size()-1)
				ret += this.get(i);
			else
				ret += this.get(i) + " ";
		}
		ret += "]]";
		return ret;
	}

	// @Override
	public boolean add(T t){
		for(int i=0 ; i<this.size() ; i++){
			if(this.get(i).compareTo(t)>=0){
				// System.out.println("insert at "+ i);
				this.add(i,t);
				return true;
			}
		}
		return super.add(t);
	}
	public int compareTo(SortedList<T> other){
		int len1 = this.size();
		int len2 = other.size();
		int len = Math.min(len1, len2);
		for(int i=0 ; i<len ; i++){
			T t1 = this.get(i);
			T t2 = other.get(i);
			if(t1.compareTo(t2)<0)
				return -1;
			else if(t1.compareTo(t2)>0)
				return 1;
		}
		if(len1<len2)
			return -1;
		else if(len1>len2)
			return 1;
		else
			return 0;
	}
};
class A implements Comparable<A>{
	Integer x;

	public A(Integer x){
		this.x = x;
	}

	// @Override
	public int compareTo(A other){
		return this.x.compareTo(other.x);
	}
	// @Override
	public String toString(){
		return "A<" + x.toString() + ">";
	}
}
class B extends A{
	Integer y;

	public B(Integer x, Integer y){
		super(x);
		this.y = y;
	}
	public Integer sum(){
		return this.x + this.y;
	}

	// @Override
	public int compareTo(B other){
		return this.sum().compareTo(other.sum());
	}
	// @Override
	public String toString(){
		return "B<" + x.toString() + "," + y.toString() + ">";
	}
}


// Main Function
public class Part1{

	public static void main(String[] args) {

		System.out.println("Hello Java!!");

		SortedList<Integer> c1 = new SortedList<Integer>();
		c1.add(5);
		c1.add(3);
		System.out.println(c1);

		A mya = new A(13);
		A myaa = new A(5);
		System.out.println(mya.compareTo(myaa));

		B myb = new B(10,20);
		B mybb = new B(30,50);
		System.out.println(myb.compareTo(mybb));
		System.out.println(mya.getClass());
		System.out.println(myb.getClass());

		SortedList<A> d1 = new SortedList<A>();
		SortedList<A> d2 = new SortedList<A>();
		for(int i = 35; i >= 0; i-=5) {
			d1.add(new A(i));
		    d2.add(new B(i+2,i+3));
		}
		d1.add(new A(7));
		d2.add(new B(20,20));
		d1.add(new A(-1));
		d2.add(new B(-3,-1));
		d1.add(new A(9999));
		d2.add(new B(9999,9999));

		addToSortedList(d1, new A(7777));
		addToSortedList(d2, new B(8888, 8888));
		System.out.println(d1);
		System.out.println(d2);

		SortedList<A> f1 = new SortedList<A>();
		addToSortedList(f1, new A(-1));
		addToSortedList(f1, new A(0));
		addToSortedList(f1, new A(4));

		System.out.println("============");
		System.out.println(d1);
		System.out.println(f1);

		System.out.println(d1.compareTo(f1));

		System.out.println("------------");
		test();
		
	}
	public static <T extends Comparable<T>> void addToSortedList(SortedList<T> l, T z){
		l.add(z);
	}
	static void test() {
		SortedList<A> c1 = new SortedList<A>();
		SortedList<A> c2 = new SortedList<A>();
		for(int i = 35; i >= 0; i-=5) {
		    addToSortedList(c1, new A(i));
		    addToSortedList(c2, new B(i+2,i+3));
		}
			
		System.out.print("c1: ");
		System.out.println(c1);
		
		System.out.print("c2: ");
		System.out.println(c2);

		switch (c1.compareTo(c2)) {
			case -1: 
			    System.out.println("c1 < c2");
			    break;
			case 0:
			    System.out.println("c1 = c2");
			    break;
			case 1:
			    System.out.println("c1 > c2");
			    break;
			default:
			    System.out.println("Uh Oh");
			    break;
		}
	}

}