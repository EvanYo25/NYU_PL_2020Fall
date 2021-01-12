// Chi-Yu Lin N16594213
// Programming Language HW4 OOP

import java.util.*;

class SortedList<T extends Comparable<T>> extends ArrayList<T> implements Comparable<SortedList<T>>{

	@Override
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

	@Override
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

	@Override
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
	Integer value;

	public A(Integer x){
		this.value = x;
	}

	@Override
	public int compareTo(A other){
		return this.value.compareTo(other.value);
	}
	@Override
	public String toString(){
		return "A<" + value.toString() + ">";
	}
}
class B extends A{
	Integer x;
	Integer y;

	public B(Integer x, Integer y){
		super(x+y);
		this.x = x;
		this.y = y;
	}
	// public Integer sum(){
	// 	return this.x + this.y;
	// }

	// @Override
	public int compareTo(B other){
		return super.compareTo(other);
		// return this.sum().compareTo(other.sum());
	}
	@Override
	public String toString(){
		return "B<" + x.toString() + "," + y.toString() + ">";
	}
}


// Main Function
public class Part1{

	public static void main(String[] args) {
		A a1 = new A(6);
    	B b1 = new B(2,4);
    	B b2 = new B(5,8);
    	System.out.println(a1.compareTo(b1)); //returns 0, since 6 = (2+4)
	    System.out.println(a1.compareTo(b2));  //returns -1, since 6 < (5+8)
	    System.out.println(b1.compareTo(a1));  //returns 0, since (2+4) = 6
	    System.out.println(b2.compareTo(a1));  //returns 1, since (5+8) > (6)
	    System.out.println(b1.compareTo(b2));  //returns -1, since (2+4) < (5+8)

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