datatype (''a, 'b) semipair = Pair of (''a * 'b option);
exception KeyError;

fun pairCount (semimap) = 
let
  fun count([], acc) = acc
    | count(Pair(_, NONE)::rest, acc) = count(rest, acc)
    | count(Pair(_, SOME _)::rest, acc) = count(rest, acc + 1);
in
  count(semimap, 0)
end;

fun singCount (semimap) = 
let
  fun count([], acc) = acc
    | count(Pair(_, NONE)::rest, acc) = count(rest, acc + 1)
    | count(Pair(_, SOME _)::rest, acc) = count(rest, acc);
in 
  count(semimap, 0)
end;

fun hasKey semimap key =
let
  fun checkKey([], _) = false
    | checkKey(Pair(k, _)::rest, key) = 
      if k = key then true 
      else checkKey(rest, key);
in
  checkKey(semimap, key)
end;

fun getItem semimap key =
let
  fun findItem([], _) = raise KeyError
    | findItem(Pair(k,v)::rest, key) =
      if k = key then Pair(k,v)
      else findItem(rest, key);
in
  findItem(semimap, key)
end;

fun getValue semimap key =
let
  fun findValue([], _) = raise KeyError
    | findValue(Pair(k,v)::rest, key) =
      if k = key then v
      else findValue(rest, key);
in
  findValue(semimap, key)
end;

fun getKeys semimap =
let
  fun collectKeys([], acc) = acc
    | collectKeys(Pair(k, _)::rest, acc) = collectKeys(rest, acc @ [k]);
in
  collectKeys(semimap, [])
end;

fun getValues semimap =
let
  fun collectValues([], acc) = acc
    | collectValues(Pair(_, NONE)::rest, acc) = collectValues(rest, acc)
    | collectValues(Pair(_, SOME v)::rest, acc) = collectValues(rest, acc @
    [v]);
in
  collectValues(semimap, [])
end;

fun insertItem (semimap: (''a, 'b) semipair list) (newItem: (''a, 'b) semipair) =
let
  fun keyExists(_, []) = false
    | keyExists(item, Pair(k, _)::rest) =
      if k = extractKey(item) then true
      else keyExists(item, rest)
  and extractKey (Pair(k, _)) = k;
in
  if keyExists(newItem, semimap)
  then
    semimap
  else
    (*List.cons(newItem, semimap)*)
    newItem::semimap
end;

fun removeItem (semimap: (''a, 'b) semipair list) key =
let
  fun removeHelper ([], acc, _) = List.rev acc
    | removeHelper (Pair(k, i)::rest, acc, key) =
      if k = key then removeHelper(rest, acc, key)
      else removeHelper(rest, Pair(k, i)::acc, key);
in
  removeHelper(semimap, [], key)
end;
