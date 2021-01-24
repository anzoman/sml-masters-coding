
val _ = print "------- reduce -------";
val _ : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a = reduce;
reduce (fn acc => fn x => 1) 0 [] = 0;
reduce (fn acc => fn x => x + 1) 0 [2, 3, 4] = 5;
reduce (fn acc => fn x => acc + x) 0 [1, 2, 3, 4, 5] = 15;

val _ = print "------- squares -------";
val _ : int list -> int list = squares;
squares [] = [];
squares [1, 2] = [1, 4] ;
squares [1, 2, 3, 4, 5] =  [1, 4, 9, 16, 25];

val _ = print "------- onlyEven -------";
val _ : int list -> int list = onlyEven;
onlyEven [] = [];
onlyEven [1, 2] = [2] ;
onlyEven [1, 2, 3, 4, 5] =  [2, 4];

val _ = print "------- bestString -------";
val _ : (string * string -> bool) -> string list -> string = bestString;
bestString (fn (s1, s2) => true) [] = "";
bestString (fn (s1, s2) => false) ["a", "ab", "abc"] = "a";
bestString (fn (s1, s2) => true) ["a", "ab", "abc"] = "abc";
bestString (fn (s1, s2) => true) ["a", "ab", "abc"] = "abc";
bestString (fn (s1, s2) => String.size s1 < String.size s2) ["aa", "a", "ab", "ac"] = "a";
bestString (fn (s1, s2) => String.size s1 > String.size s2) ["a", "ab", "abcd", "abc"] = "abcd";

val _ = print "------- largestString -------";
val _ : string list -> string = largestString;
largestString [] = "";
largestString ["", "", ""] = "";
largestString ["a","b","c","d"] = "d";
largestString ["a","x","c","d"] = "x";
largestString ["abc","abd","abe"] = "abe";
largestString ["aa","aaa","aaaa"] = "aaaa";
largestString ["jupi","jipi","japi", "jepi"] = "jupi";

val _ = print "------- longestString -------";
val _ : string list -> string = longestString;
longestString [] = "";
longestString ["", "", ""] = "";
longestString ["a","b","c","d"] = "a";
longestString ["a","x","c","d"] = "a";
longestString ["abc","abd","abe"] = "abc";
longestString ["aa","aaa","aaaa"] = "aaaa";
longestString ["123","12","123456", "1258"] = "123456";

val _ = print "------- quicksort -------";
val _ : ('a * 'a -> order) -> 'a list -> 'a list = quicksort;
quicksort Int.compare [] = [];
quicksort Int.compare [10] = [10];
quicksort Int.compare [1, 5] = [1, 5];
quicksort Int.compare [22, 21] = [21, 22];
quicksort Int.compare [20, 30, 10] = [10, 20, 30];
quicksort Int.compare [20, ~30, 10] = [~30, 10, 20];
quicksort Int.compare [5, 4, 3, 2, 1] = [1, 2, 3, 4, 5];
quicksort Int.compare [33, 15, 20, 15, 20, 15, 20, 33] = [15, 15, 15, 20, 20, 20, 33, 33];
quicksort Int.compare [55, ~22, 8, 7, ~7, 0, ~111, 99] = [ ~111, ~22, ~7, 0, 7, 8, 55, 99];
quicksort String.compare ["a", "abc", "lala"] = ["a", "abc", "lala"];

val _ = print "------- dot -------";
val _ : int list -> int list -> int = dot;
dot [] [] = 0;
dot [] [1] = 0;
dot [1] [1] = 1;
dot [5] [4] = 20;
dot [1, 2] [2, 3] = 8;
dot [1, ~1] [~2, 4] = ~6;
dot [3, 5, 2] [1, 1, 1] = 10;

val _ = print "------- transpose -------";
val _ : 'a list list -> 'a list list = transpose;
transpose [] = [];
transpose [[1]] = [[1]];
transpose [[1, 2, 3]] = [[1], [2], [3]];
transpose [[1, 11], [2, 22], [3, 33]] = [[1, 2, 3], [11, 22, 33]];
transpose [[1, 2, 3], [4, 5, 6], [7, 8, 9]] = [[1, 4, 7], [2, 5, 8], [3, 6, 9]];

val _ = print "------- multiply -------";
val _ : int list list -> int list list -> int list list = multiply;
multiply [] [] = [];
multiply [[1, 3], [2, 4]] [[1], [2]] = [[7], [10]];
multiply [[1, 2], [3, 4]] [[1], [2]] = [[5], [11]];
multiply [[1, 2], [3, 4]] [[1, 2], [3, 4]] = [[7, 10], [15, 22]];

val _ = print "------- group -------";
val _ : ''a list -> (''a * int) list = group;
group [] = [];
group [1] = [(1, 1)];
group [1, 2, 3] = [(1, 1), (2, 1), (3, 1)];
group [1, 1, 1, 1, 2, 3, 3, 5] = [(1, 4), (2, 1), (3, 2), (5, 1)];
group [1, 1, 1, 2, 3, 3, 5, 1] = [(1, 3), (2, 1), (3, 2), (5, 1), (1, 1)];
group ["a", "a", "b", "c", "c", "c"] = [("a", 2), ("b", 1), ("c", 3)];

val _ = print "------- equivalenceClasses -------";
val _ : ('a -> 'a -> bool) -> 'a list -> 'a list list = equivalenceClasses;
equivalenceClasses (fn a => fn b => true) [] = [];
equivalenceClasses (fn a => fn b => a mod 2 = b mod 2) [1, 2] = [[1], [2]];
equivalenceClasses (fn a => fn b => a mod 2 = b mod 2) [2, 2, 2] = [[2, 2, 2]];
equivalenceClasses (fn a => fn b => a mod 3 = b mod 3) [1, 2, 3, 4, 5, 6] = [[1, 4], [2, 5], [3, 6]];
equivalenceClasses (fn a => fn b => a = true andalso b = true) [true, false, true] = [[true, true], [false]];
