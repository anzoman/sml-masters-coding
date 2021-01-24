(* Podan seznam xs agregira z začetno vrednostjo z in funkcijo f v vrednost f (f (f z s_1) s_2) s_3) ... *)
(* Aggregates xs with an initial value z and function f and returns f (f (f z s_1) s_2) s_3) ... *)
fun reduce f z s = List.foldl (fn (x, acc) => f acc x) z s;

(* Vrne seznam, ki vsebuje kvadrate števil iz vhodnega seznama. Uporabite List.map. *)
(* Returns a list of squares of the numbers. Use List.map. *)
fun squares s = List.map (fn x => x * x) s; 

(* Vrne seznam, ki vsebuje vsa soda števila iz vhodnega seznama. Uporabite List.filter. *)
(* Returns a list that contains only even numbers from xs. Use List.filter. *)
fun onlyEven s = List.filter (fn x => x mod 2 = 0) s;

(* Vrne najboljši niz glede na funkcijo f (prvi arg.). Funkcija f primerja dva niza in vrne true, če je prvi niz boljši od drugega. Uporabite List.foldl. Najboljši niz v praznem seznamu je prazen niz. *)
(* Returns the best string according to the function f (first arg.). The function f compares two strings and returns true if the first string is better than the other. Use List.foldl. The best string in an empty list is an empty string. *)
fun bestString f s =
  case s of 
    [] => ""
  | h::t => List.foldl (fn (x, acc) => if f (x, acc) then x else acc) h t;

(* Vrne leksikografsko največji niz. Uporabite bestString. *)
(* Returns the largest string according to alphabetical ordering. Use bestString. *)
fun largestString s = bestString (fn (a, b) => a > b) s

(* Vrne najdaljši niz. Uporabite bestString. *)
(* Returns the longest string. Use bestString. *)
fun longestString s = bestString (fn (s1, s2) => String.size s1 > String.size s2) s

(* Seznam uredi naraščajoče z algoritmom quicksort. Prvi argument je funkcija za primerjanje. *)
(* Sorts the list with quicksort. First argument is a compare function. *)
fun quicksort f s = 
  case s of
    [] => []
  | h::t => (let val (l, r) = List.partition (fn x => f (x, h) = LESS) t
             in quicksort f l @ [h] @ quicksort f r 
             end);

(* Vrne skalarni produkt dveh vektorjev. Uporabite List.foldl in ListPair.map. *)
(* Returns the scalar product of two vectors. Use List.foldl and ListPair.map. *)
fun dot v1 v2 = List.foldl (fn ((a, b), acc) => acc + (a * b)) 0 (ListPair.zip (v1, v2))

(* Vrne transponirano matriko. Matrika je podana z vrstičnimi vektorji od zgoraj navzdol:
  [[1,2,3],[4,5,6],[7,8,9]] predstavlja matriko
   [ 1 2 3 ]
   [ 4 5 6 ]
   [ 7 8 9 ]
*)
(* Returns the transpose of m. The matrix m is given with row vectors from top to bottom:
  [[1,2,3],[4,5,6],[7,8,9]] represents the matrix
   [ 1 2 3 ]
   [ 4 5 6 ]
   [ 7 8 9 ]
*)
fun transpose m = if null m orelse null (hd m) then [] else (List.map hd m) :: transpose (List.map tl m);

(* Zmnoži dve matriki. Uporabite dot in transpose. *)
(* Multiplies two matrices. Use dot and transpose. *)
fun multiply m1 m2 = List.map (fn a => List.map (fn b => (dot a b)) (transpose m2)) m1;

(* V podanem seznamu prešteje zaporedne enake elemente in vrne seznam parov (vrednost, število ponovitev). Podobno deluje UNIX-ovo orodje uniq -c. *)
(* Counts successive equal elements and returns a list of pairs (value, count). The unix tool uniq -c works similarly. *)
fun group s = 
	case s of
		[] => []
  | h::_ => let val n = List.foldl(fn ((x, y), acc) => if x = h andalso acc = (y - 1) then acc + 1 else acc) 1 (ListPair.zip (s, List.tabulate (List.length s, fn e => e + 1)));
            in
              (h, n) :: group (List.drop (s, n))
            end;

(* Elemente iz podanega seznama razvrsti v ekvivalenčne razrede. Znotraj razredov naj bodo elementi v istem vrstnem redu kot v podanem seznamu. Ekvivalentnost elementov definira funkcija f, ki za dva elementa vrne true, če sta ekvivalentna. *)
(* Sorts the elements from a list into equivalence classes. The order of elements inside each equivalence class should be the same as in the original list. The equivalence relation is given with a function f, which returns true, if two elements are equivalent. *)
fun equivalenceClasses f s = 
  case s of
    [] => []
  | h::t => let val (p1, p2) = List.partition (fn e => f h e) t;
            in (h :: p1) :: (equivalenceClasses f p2)
            end;
