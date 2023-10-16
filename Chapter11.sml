(*Chapter 11 problem 3*)
(*Having to rename from INT to NUM here as the intnest INT will shadow this
* definition if its the same*)
datatype number = NUM of int | REAL of real;

(*Chapter 11 problem 4*)
fun plus (NUM x) (NUM y) = NUM (x + y)
  | plus (REAL x) (REAL y) = REAL( x + y)
  | plus (NUM x) (REAL y) = REAL(real x + y)
  | plus (REAL x) (NUM y) = REAL(x + real y);

(*Chapter 11 problem 5*)
datatype intnest = INT of int | LIST of intnest list;

fun addup (INT n) = n
  | addup (LIST []) = 0
  | addup (LIST (n::ns)) = addup n + addup (LIST ns);

(*Chapter 11 problem 9*)
datatype 'data tree = Empty | Node of 'data tree * 'data * 'data tree;

fun appendall Empty = nil
  | appendall (Node(left, l, right)) = appendall left @ l @ appendall right;

(*Chapter 11 problem 10*)
fun isComplete Empty = true
  | isComplete (Node (left, _, right)) = 
        case (left, right) of
             (Empty, Empty) => true
           | (Node(_, _, _), Empty) => false
           | (Empty, Node(_, _, _)) => false
           | (Node(_, _, _), Node(_, _, _)) => isComplete left andalso
           isComplete right;
