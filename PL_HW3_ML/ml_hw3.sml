Control.Print.printDepth := 100;
Control.Print.printLength := 100;

(* Q1 *)
fun foo f g x = [g [f x]]

(*-------------------------------------------*)
(* Q2 *)
fun bar x = fn L =>
	let fun f [] = []
		 |	f (y::ys) = (x * y) :: f ys
	in f L end

(* Q2 test *)
(*val g = bar 30;
g [1,2,3,4,5];
bar 30 [1,2,3,4,5];*)

(*-------------------------------------------*)
(* Q3 *)
fun part x L= 
	let fun loop (y,z,nil) = (rev y, rev z)
		 |	loop (y,z,w::ws) = if w < x then loop (w :: y, z , ws)
		 								else loop (y, w :: z , ws)
	in loop ([],[],L) end
(* Q3 test *)
(*part 6 [5,2,8,4,1,9,6,10];*)

(*-------------------------------------------*)
(* Q4 *)
fun partSort [] = []
 |	partSort [x] = [x]
 |	partSort (x::xs) = 
 		let val (l1,l2) = (part x xs)
 		in	(partSort l1) @ (x :: (partSort l2)) end
(* Q4 test *)
(*partSort [5,2,9,10,12,4,8,1,19]*)

(*-------------------------------------------*)
(* Q5 *)
fun pSort (op <) [] = []
 |	pSort (op <) [k] = [k]
 |	pSort (op <) (k::ks) = 
 		let fun part2 x L=
 				let fun loop (y,z,nil) = (rev y, rev z)
 					 |	loop (y,z,w::ws) = if (w < x)	then loop (w :: y, z, ws)
 					 									else loop (y, w :: z, ws)
 				in loop ([],[],L) end
 			val (l1,l2) = (part2 k ks)
 		in (pSort (op <) l1) @ (k :: (pSort (op <) l2)) end
(* Q5 test *)
(*pSort (op <) [1,9, 3, 6, 7]*)
(*pSort (fn(a,b) => length a < length b) [[1, 9, 3, 6], [1], [2,4,6], [5,5]]*)

(*-------------------------------------------*)
(* Q6 *)
exception reduce_error
fun reduce f [] = raise reduce_error
 |	reduce f [x] = x
 |	reduce f (x::xs) = f x (reduce f xs)
(* Q6 test *)
(*fun g x y = x + y;
reduce g  [1,2,3,4,5];
reduce (fn x => fn y => x + y) [1,2,3,4,5];*)

(*-------------------------------------------*)
(* Q7 *)
datatype 'a tree = leaf of 'a | node of 'a tree list
(* Q7 test *)
(*val myTree = node [node [node [leaf [4,2,14],leaf [9,83,32],leaf [96,123,4]],node [leaf [47,71,82]],node [leaf [19,27,10],leaf [111,77,22,66]],leaf [120,42,16]],leaf [83,13]]*)

(*-------------------------------------------*)
(* Q8 *)
fun fringe (leaf t) = [t]
 |	fringe (node []) = []
 |	fringe (node n) = (reduce (fn x => fn y => x @ y) (map fringe n))
(* Q8 test *)
(*fringe (node [leaf 1,node [leaf 2,leaf 3],node [node [leaf 4,leaf 5],leaf 6]])*)
(*fringe myTree*)

(*-------------------------------------------*)
(* Q9 *)
fun sortTree (op <) (leaf t) = leaf (pSort (op <) t)
 |	sortTree (op <) (node []) = node []
 |	sortTree (op <) (node x) = node (map (sortTree (op <)) x)
(* Q9 test *)
 (*sortTree (op <) myTree;*)

(*-------------------------------------------*)
(* Q10 *)
fun powerSet [] = [[]]
 |	powerSet L = 
	 	let fun loop (x,nil) = x
			 |	loop (x,y::ys) = 
			 		let val exists = x
			 		in loop (x @ (map (fn i => i @ [y]) exists), ys) end
		in loop ([[]],L) end
(* Q10 test *)
(*powerSet [1,2,3];*)

(*-------------------------------------------*)


fun rotate [] = []
 |	rotate [x] = [x]
 |	rotate (x::xs) = 
 		let val (y::ys) = rotate xs
 		in	y::x::ys
 		end

fun bar w x (y,z) =
     let val (b::bs) = w y
         val d = x z
         fun f a b = length a + b
     in f b d
end


fun greaterThan3 x = x > 3
fun filter f [] = []
 |	filter f (x::xs) = 
 		if f x 	then x::(filter f xs)
 				else (filter f xs)


fun f (x,y) [] = []
 |	f (x,y) (z::zs) = if length y = 1 	then y
 										else z


fun decPrefix [] = []
 |	decPrefix [x] = [x]
 |	decPrefix (x::xs) = 
 		let val (y::ys) = decPrefix xs
 		in if (x > y) then x::y::ys
 						else [x]
 		end



