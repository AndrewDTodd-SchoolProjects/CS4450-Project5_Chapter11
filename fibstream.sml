use "stream.sml";

(*Function to generate an infinite stream of the fibonacci numbers*)
fun fib () = 
    let
       fun fibfrom 0 = 0
         | fibfrom 1 = 1
         | fibfrom n = fibfrom (n - 1) + fibfrom (n - 2)

      fun genFibStream n = Cons(fibfrom n, fn () => genFibStream (n + 1))
    in
       genFibStream 0
    end;
