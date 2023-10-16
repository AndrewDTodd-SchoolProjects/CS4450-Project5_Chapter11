datatype entry = File of string | Folder of string * entry list;

val files = Folder("d1",
	[File "f1",
	Folder("d2",
		[File "f2",
		Folder("d3",
			[File "f3"])]),
	File "f4",
	Folder("d3",
		[File "f5"])]);

(*fun get_entries (File fname) = [fname]
|   get_entries (Folder(dname, cnts)) = dname::(get_contents cnts)
and
	get_contents L = foldr (fn (e,sofar) => get_entries e @ sofar) [] L;

get_entries files;*)

(*Additional code for additional problem A*)
fun print_entries (File name) = (print(name ^ "\n"))
  | print_entries (Folder (name, contents)) = (print(name ^ "\n");
  print_contents contents)
and
    print_contents [] = ()
  | print_contents (x::xs) = (print_entries x; print_contents xs);
