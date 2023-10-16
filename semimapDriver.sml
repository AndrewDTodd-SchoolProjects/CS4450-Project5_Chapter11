use "semimap.sml";

(* Tests for semimap *)
val stuff = [(Pair (1, SOME "one")), Pair (2, NONE)];
pairCount stuff;
singCount stuff;
getItem stuff 1;
getItem stuff 10 handle KeyError => Pair (0, SOME "error!");
getValue stuff 1;
getValue stuff 10 handle KeyError => SOME "error!";
getKeys stuff;
getValues stuff;
hasKey stuff 1;
hasKey stuff 3;
pairCount stuff;
singCount stuff;
val stuff2 = insertItem stuff (Pair (3, SOME "three"));
val stuff3 = insertItem stuff2 (Pair (3, SOME "three"));
val stuff4 = removeItem stuff3 1;
